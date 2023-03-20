library(data.table)
library(dplyr)
library(plyr)
library(tidyverse)
library(multcomp)
library(lme4)
library(lmerTest)
############################import data#######################################
data = fread(file ='C:/Users/wyf/Desktop/internship/SMILE DATA/SMILE-DATA.csv',sep=',')

#demographic
demo = fread(file = 'C:/Users/wyf/Desktop/internship/SMILE DATA/SMILE_demo.csv',sep=',')

#SCL90
SCL = fread(file = 'C:/Users/wyf/Desktop/internship/SMILE DATA/SMILE-SCL90.csv',sep=',')
SCL = SCL[,c('record_id','sum_score')]
SCL$sum_score = SCL$sum_score/90

#drop irrelevant NAs
data = data[!is.na(data$redcap_repeat_instance),]

data$esm_time =as.POSIXct(data$esm_time, format = '%m/%d/%Y %H:%M')


###########################identify the first stress event####################

#create an empty data frame for data cleaning

#days with stress event
stress_data = data.frame()

#corresponding NA mean in each beep
NA_each_beep = data.frame()

#the position of first stress event each day
first_stress = c()

id = vector()

all_stressor = data.frame()

#compute the days in total

days = length(data$record_id)/12
number_participants = length(unique(data$record_id))
record_id = unique(data$record_id)


for (i in 1:days){
  
  #separate the i th day data, discarding morning & evening
  data_today = data[((12*(i-1)+2):(12*i-1)),]
  
  first_stress[i] = which(data_today$esm_event_pleasant < 0)[1]
  
  #if there is no stress event or the event happened on the first beep
  if (is.na(first_stress[i]) | first_stress[i] == 1 ){
    
    add_stress_data = data.frame()
    all_stressor[i,] = rep(NA,10)
    
    #if there is no NA data avaliable on beep(t-1)
  }else{
    
    if (is.na(data_today$esm_mood_worry[first_stress[i]-1])){
      add_stress_data = data.frame()
      all_stressor[i,] = rep(NA,10)
      
    #include other data
    }else{
  
    add_stress_data = data_today[(first_stress[i]-1):10,]
    id[i] = as.numeric(add_stress_data[1,1])
    all_stressor[i,1:10] = as.numeric(t(data_today$esm_event_pleasant))
    #compute NA in every beep
    NA_each_beep_participant =c()
    for (k in 1:10){
    NA_each_beep_participant[k] = mean(c(add_stress_data[k,]$esm_mood_worry,add_stress_data[k,]$esm_mood_stressed,add_stress_data[k,]$esm_mood_anxious,
                    add_stress_data[k,]$esm_mood_irritated,add_stress_data[k,]$esm_mood_down,add_stress_data[k,]$esm_mood_restless,add_stress_data[k,]$esm_mood_tense),na.rm=T)
    }
    
    NA_each_beep = rbind.fill(NA_each_beep,as.data.frame(t(NA_each_beep_participant)))
    
   
    
  }
  }
  stress_data = rbind(stress_data,add_stress_data)
}

all_stressor=all_stressor[rowSums(is.na(all_stressor)) != ncol(all_stressor), ]
all_stressor[all_stressor >= 0] <- NA    

for (i in 1:168){
  
    all_stressor[i,][which(!is.na(all_stressor[i,]))[1]] = NA
}

all_stressor[!is.na(all_stressor)] = 1
all_stressor[is.na(all_stressor)] = 0
all_stressor$record_id =id[!is.na(id)]

all_stressor = all_stressor %>% pivot_longer(cols=c("V1",'V2', 'V3','V4','V5','V6','V7','V8','V9','V10'),
                                                                             names_to='time',
                                                                                    values_to='extra_stressor')


all_stressor=all_stressor %>% dplyr::select(  -time )



NA_each_beep$record_id =id[!is.na(id)]

setnames(NA_each_beep, old=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","record_id"), 
         new=c("time_minus1", "time_0","time_1", "time_2",
               "time_3","time_4", "time_5","time_6",
               "time_7","time_8","record_id"))



#in how many days the stress recovery has been reported
stress_days = first_stress[!is.na(first_stress)]
stress_ratio = length(stress_days)/days



#assign groups according to SCL-90
SCL = SCL[SCL$record_id %in% unique(NA_each_beep$record_id)]
#hist(SCL$sum_score)



#analyze using contrast

NA_each_beep = merge(NA_each_beep,SCL,by='record_id')
NA_each_beep = merge(NA_each_beep,demo,by='record_id')

#count the non-na number
168-sum(is.na(NA_each_beep$time_1))
168-sum(is.na(NA_each_beep$time_2))
168-sum(is.na(NA_each_beep$time_3))
168-sum(is.na(NA_each_beep$time_4))
168-sum(is.na(NA_each_beep$time_5))
168-sum(is.na(NA_each_beep$time_6))
168-sum(is.na(NA_each_beep$time_7))
168-sum(is.na(NA_each_beep$time_8))


#select SCL groups
SCL_lower = SCL$record_id[which(SCL$sum_score<0.3389)]
SCL_higher = SCL$record_id[which(SCL$sum_score>0.7944)]

NA_lm = NA_each_beep %>% pivot_longer(cols=c("time_minus1",'time_0', 'time_1','time_2','time_3',
                                             'time_4','time_5','time_6','time_7',
                                             'time_8'),
                                      names_to='time',
                                      values_to='negaff')

NA_lm$extra_stressor=all_stressor$extra_stressor

NA_lm_lower = NA_lm[NA_lm$record_id %in% SCL_lower,] 

NA_lm_higher = NA_lm[NA_lm$record_id %in% SCL_higher,] 


#analyze using contrast, build a linear model
NA_lm_lower$time <- relevel(factor(NA_lm_lower$time), ref="time_minus1")
NA_lm_lower = NA_lm_lower[!is.na(NA_lm_lower$negaff),]
mod_lower = lmer(negaff ~ factor(time) + demo_age  + (1 +factor(time)| record_id) + extra_stressor,data=NA_lm_lower)
summary(mod_lower)
anova(mod_lower)

NA_lm_higher$time <- relevel(factor(NA_lm_higher$time), ref="time_minus1")
NA_lm_higher = NA_lm_higher[!is.na(NA_lm_higher$negaff),]
mod_higher = lmer(negaff ~ factor(time) + demo_age +(1 + factor(time)| record_id)+ extra_stressor,data=NA_lm_higher)
summary(mod_higher)

NA_lm$time <- relevel(factor(NA_lm$time), ref="time_minus1")
mod = lmer(negaff ~ factor(time)  + sum_score +
           factor(time) * sum_score + demo_age + demo_sex +extra_stressor +(1 + factor(time)|record_id),data=NA_lm)
summary(mod)

#visualization
library(plotly)

used_data = data.frame(table(id[!is.na(id)]))
full_data = data.frame(table(data$record_id))
full_data$Freq = full_data$Freq/12

visual = merge(used_data,full_data,by='Var1',all=T)
visual[is.na(visual)] <- 0
visual$Freq.y = visual$Freq.y-visual$Freq.x

names(visual)[names(visual) == 'Var1'] <- 'participant'
names(visual)[names(visual) == 'Freq.x'] <- 'included'
names(visual)[names(visual) == 'Freq.y'] <- 'excluded'

visual = visual %>% pivot_longer(cols=c("included",'excluded'),
                                 names_to='data.usage',
                                 values_to='days')


ggplot(visual, aes(x = participant, y=days, fill = data.usage)) + 
  geom_bar(stat = "identity")+
  scale_fill_brewer()+
  ggtitle("Model with NA-1")



used_data_baseline = data.frame(table(id[!is.na(id)]))
visual = merge(visual,used_data_baseline,by='Var1')

visual[is.na(visual)] <- 0
visual$Freq.y = visual$Freq.y-visual$Freq.x
visual$Freq = visual$Freq-visual$Freq.x
visual$Freq.y = visual$Freq.y-visual$Freq

names(visual)[names(visual) == 'Var1'] <- 'participant'
names(visual)[names(visual) == 'Freq.x'] <- 'NA-1'
names(visual)[names(visual) == 'Freq.y'] <- 'excluded'
names(visual)[names(visual) == 'Freq'] <- 'NAm'
visual = visual %>% pivot_longer(cols=c("NA-1",'NAm','excluded'),
                                 names_to='data.usage',
                                 values_to='days')
visual$data.usage <- relevel(factor(visual$data.usage), ref="excluded")

ggplot(visual, aes(x = participant, y=days, fill = data.usage)) + 
  geom_bar(stat = "identity")+
  scale_fill_brewer()+
  ggtitle("Model with NA-1")
