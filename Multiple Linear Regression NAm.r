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


data = separate(data, esm_time, into = c("Date", "beeptime"), sep = " (?=[^ ]+$)")
data = separate(data, beeptime, into = c("hour", "minute"), sep = ":")
data$beeptime = as.numeric(data$hour) * 60 + as.numeric(data$minute) 
data = data[,-c('hour','minute','Date')]


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

extra_stressor = vector()

dayno = vector()
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
    extra_stressor[i] = NA
    dayno[i] = NA
    
  
  }else{
    
    add_stress_data = data_today
    id[i] = as.numeric(add_stress_data[1,1])
    
    dayno[i] = as.numeric(add_stress_data$esm_dayno[1])
    
    #compute NA in every beep
    NA_each_beep_participant =c()
    for (k in 1:10){
      NA_each_beep_participant[k] = mean(c(add_stress_data[k,]$esm_mood_worry,add_stress_data[k,]$esm_mood_stressed,add_stress_data[k,]$esm_mood_anxious,
                                           add_stress_data[k,]$esm_mood_irritated,add_stress_data[k,]$esm_mood_down,add_stress_data[k,]$esm_mood_restless,add_stress_data[k,]$esm_mood_tense),na.rm=T)
    }
    NA_each_beep = rbind.fill(NA_each_beep,as.data.frame(t(NA_each_beep_participant)))
    
    if(length(which(data_today$esm_event_pleasant < 0))>1){
      
      extra_stressor[i] = 1
      
    }else{
      
      extra_stressor[i] = 0
      
    }
  }
  
  stress_data = rbind(stress_data,add_stress_data)
}





NA_each_beep$record_id = id[!is.na(id)]
NA_each_beep$dayno = dayno[!is.na(dayno)]
NA_each_beep$first_stressor = first_stress[!is.na(first_stress)]
NA_each_beep$extra_stressor = extra_stressor[!is.na(extra_stressor)]

setnames(NA_each_beep, old=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","record_id","dayno","first_stressor","extra_stressor"), 
         new=c("beep_0","beep_1", "beep_2",
               "beep_3","beep_4", "beep_5","beep_6",
               "beep_7","beep_8","beep_9","record_id","dayno","first_stressor","extra_stressor"))


#in how many days the stress recovery has been reported
stress_days = first_stress[!is.na(first_stress)]
stress_ratio = length(stress_days)/days

#how many NA(t-1) exist in the data set
#NA_baseline_auto_ratio = length(NA_baseline_auto[!is.na(NA_baseline_auto)])/length(stress_days)


#assign groups according to SCL-90
SCL = SCL[SCL$record_id %in% unique(NA_each_beep$record_id)]

hist(SCL$sum_score)


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





NA_each_beep = merge(NA_each_beep,SCL,by='record_id')
NA_each_beep = merge(NA_each_beep,demo,by='record_id')
#analyze using contrast
summary(SCL$sum_score)

SCL_lower = SCL$record_id[which(SCL$sum_score<0.3389)]
SCL_higher = SCL$record_id[which(SCL$sum_score>0.7944)]

NA_each_beep=NA_each_beep %>% dplyr:::select(-record_id, record_id)



#initial reactivity and duration
for (i in 1:length(NA_each_beep$record_id)){
  NA_each_beep$reactivity[i] = NA_each_beep[i,(NA_each_beep$first_stressor[i])] - NA_each_beep$baseline[i]
#discard no initial reactivity
  full_data = stress_data[((10*(i-1)+1):(10*i)),]
  stress_beep = NA_each_beep$first_stressor[i]
  recovery_beep = which(NA_each_beep[i,((NA_each_beep$first_stressor[i]):10)]<=NA_each_beep$baseline[i])[1]+stress_beep-1
    if (is.na(recovery_beep)){
      NA_each_beep$minute[i] = full_data$beeptime[10]-full_data$beeptime[stress_beep]
 NA_each_beep$minute[i] = NA
    }else{
     NA_each_beep$minute[i] = full_data$beeptime[recovery_beep]-full_data$beeptime[stress_beep]
    }
}

NA_each_beep <- NA_each_beep[, c("minute", "demo_age", "demo_sex","reactivity","sum_score","record_id","extra_stressor")]
NA_each_beep <- NA_each_beep[!is.na(NA_each_beep$minute),]



mod = lmerTest::lmer(data=NA_each_beep,minute ~ sum_score  + demo_age + demo_sex +
           extra_stressor + (1|record_id))
summary(mod)

mod1 = lm(data=NA_each_beep,minute ~ sum_score  + demo_age + demo_sex +
           extra_stressor )
summary(mod1)



mod1 = lm(data=NA_each_beep,minute ~ sum_score  + demo_age + demo_sex +
           extra_stressor + reactivity )
summary(mod1)

mod2 = lm(data=NA_each_beep,minute ~  demo_age + demo_sex +
            extra_stressor + reactivity )
summary(mod2)


