library(data.table)
library(dplyr)
library(plyr)
library(tidyverse)
library(multcomp)

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

#participant-wise NA baseline
NA_baseline_mean = c()


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
  if (is.na(first_stress[i]) ){
    
    add_stress_data = data.frame()
    all_stressor[i,] = rep(NA,10)
    
    #if there is no NA data avaliable on beep(t-1)
  }else{
    
      add_stress_data = data_today[(first_stress[i]):10,]
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
  
  stress_data = rbind(stress_data,add_stress_data)
}

all_stressor=all_stressor[rowSums(is.na(all_stressor)) != ncol(all_stressor), ]
all_stressor[all_stressor >= 0] <- NA    

for (i in 1:257){
  
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
         new=c("time_0","time_1", "time_2",
               "time_3","time_4", "time_5","time_6",
               "time_7","time_8","time_9","record_id"))


#in how many days the stress recovery has been reported
stress_days = first_stress[!is.na(first_stress)]
stress_ratio = length(stress_days)/days


#assign groups according to SCL-90
SCL = SCL[SCL$record_id %in% unique(NA_each_beep$record_id)]

hist(SCL$sum_score)

#identify two SCL group
lower_SCL_group = SCL$record_id[SCL$sum_score<=as.numeric(summary(SCL$sum_score)[3])]
higher_SCL_group = SCL$record_id[SCL$sum_score>as.numeric(summary(SCL$sum_score)[3])]

for (i in 1:length(NA_each_beep$record_id)){
  if (NA_each_beep$record_id[i] %in% lower_SCL_group){
    NA_each_beep$group[i] = 1
  }else{
    NA_each_beep$group[i] = 2
  }
}


#compute personal mean
data_NA = data %>%
  dplyr::select(record_id,esm_mood_worry,esm_mood_stressed,esm_mood_anxious,
         esm_mood_irritated,esm_mood_down,esm_mood_restless,esm_mood_tense)

data_NA$mean = rowMeans(data_NA[,2:8])

data_NA = data_NA[!is.na(data_NA$mean),]

NA_baseline_mean = setDT(data_NA)[, mean(mean), by = record_id]

NA_baseline_mean$record_id = record_id

NA_each_beep =merge(NA_each_beep,NA_baseline_mean,by='record_id')

#NA_each_beep$again = extra_stressor[!is.na(extra_stressor)]
names(NA_each_beep)[names(NA_each_beep)=="V1"] <- "baseline"




#identify two SCL group
lower_SCL_group = SCL$record_id[SCL$sum_score<=as.numeric(summary(SCL$sum_score)[3])]
higher_SCL_group = SCL$record_id[SCL$sum_score>as.numeric(summary(SCL$sum_score)[3])]

for (i in 1:length(NA_each_beep$record_id)){
  if (NA_each_beep$record_id[i] %in% lower_SCL_group){
    NA_each_beep$group[i] = 1
  }else{
    NA_each_beep$group[i] = 2
  }
}



NA_each_beep = merge(NA_each_beep,SCL,by='record_id')
NA_each_beep = merge(NA_each_beep,demo,by='record_id')
#analyze using contrast
summary(SCL$sum_score)

SCL_lower = SCL$record_id[which(SCL$sum_score<0.3389)]
SCL_higher = SCL$record_id[which(SCL$sum_score>0.7944)]


#analyze using contrast, build a linear model



NA_lm = NA_each_beep %>% pivot_longer(cols=c('time_0', 'time_1','time_2','time_3',
                                              'time_4','time_5','time_6','time_7',
                                              'time_8','time_9','baseline'),
                                              names_to='time',
                                              values_to='negaff')
NA_lm$extra_stressor=rep(NA,2827)

p=0

for (i in 1:257){
  
  NA_lm$extra_stressor[(10*(i-1)+1+p): ((10*i)+p)] = all_stressor$extra_stressor[(10*(i-1)+1): (10*i)]
  p=p+1
}

NA_lm$extra_stressor[is.na(NA_lm$extra_stressor)] = 0


NA_lm_lower = NA_lm[NA_lm$record_id %in% SCL_lower,] 

NA_lm_higher = NA_lm[NA_lm$record_id %in% SCL_higher,] 

NA_lm_lower$time <- relevel(factor(NA_lm_lower$time), ref="baseline")

mod_lower = lmer(negaff ~ factor(time) + demo_age + (1 + factor(time)| record_id) + extra_stressor,data=NA_lm_lower)
summary(mod_lower)

NA_lm_higher$time <- relevel(factor(NA_lm_higher$time), ref="baseline")
mod_higher = lmer(negaff ~ factor(time) + demo_age + (1 + factor(time)| record_id)+ extra_stressor,data=NA_lm_higher)
summary(mod_higher)

NA_lm$time <- relevel(factor(NA_lm$time), ref="baseline")
mod = lmer(negaff ~ factor(time)  + sum_score +
           factor(time) * sum_score + demo_age + demo_sex+extra_stressor + (1+factor(time)|record_id),data=NA_lm)
summary(mod)

###further analysis based on NA(t-1), NAt, and NA(t+1)
NA_inter_lower = NA_lm_lower[NA_lm_lower$time %in% c('baseline','time_0','time_1'),]
NA_inter_higher = NA_lm_higher[NA_lm_higher$time %in% c('baseline','time_0','time_1'),]
NA_inter = rbind(NA_inter_lower,NA_inter_higher)

mod_inter = lmer(NA_inter$negaff ~ factor(NA_inter$time) + factor(NA_inter$group)+ factor(NA_inter$time) * 
                 factor(NA_inter$group) +(1+factor(NA_inter$time)|NA_inter$record_id)+NA_inter$demo_age+
                   NA_inter$extra_stressor)

summary(mod_inter)

NA_inter=NA_inter[!is.na(NA_inter$negaff),]

myplot=interaction.plot(x.factor = factor(NA_inter$time),
                 trace.factor = factor(NA_inter$group), 
                 response = NA_inter$negaff, fun = mean,
                 xlab = 'time point',
                 ylab = 'NA',
                 trace.label = 'SCL',
                 col=c("blue","red"))




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
  ggtitle("Model with NAm")


