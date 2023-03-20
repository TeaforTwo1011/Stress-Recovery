library(data.table)
library(dplyr)
library(plyr)
library(tidyverse)
library(survival)

############################import data#######################################
#esm
dat = fread(file ='C:/Users/wyf/Desktop/internship/SMILE DATA/SMILE-DATA - survival ginette.csv',sep=',')
dat = dat[!is.na(dat$beepno),]
dat = dat[dat$beepno %in% c(1:10)]

#reconstruct beeptime
dat = separate(dat, beeptime, into = c("Date", "beeptime"), sep = " (?=[^ ]+$)")
dat = separate(dat, beeptime, into = c("hour", "minute"), sep = ":")
dat$beeptime = as.numeric(dat$hour) * 60 + as.numeric(dat$minute) 
dat = dat[,-c('hour','minute','Date')]

#reconstruct na
for (i in 1:length(dat$subjno)){
  dat$na[i] = mean(as.numeric(dat[i,c('mood_worry','mood_stressed','mood_anxious','mood_irritated','mood_down','mood_restless','mood_tense')]),na.rm=T)
}

#assign a status
dat$status = rep(1,time=length(dat$subjno))

#demographic
demo = fread(file = 'C:/Users/wyf/Desktop/internship/SMILE DATA/SMILE_demo.csv',sep=',')
demo$subjno = demo$record_id
demo$record_id = NULL

#SCL90
SCL = fread(file = 'C:/Users/wyf/Desktop/internship/SMILE DATA/SMILE-SCL90.csv',sep=',')
SCL = SCL[,c('record_id','sum_score')]
SCL$id = SCL$record_id
SCL$sum_score = SCL$sum_score/90
SCL$record_id = NULL

dat = merge(dat, demo, by='subjno')

###########################code from ginette#################################


names(dat)[names(dat)=="demo_age"] <- "age"
names(dat)[names(dat)=="demo_sex"] <- "sex"

# rename na into negaff 
names(dat)[names(dat) == "na"] <- "negaff"

# rename status2 into group
names(dat)[names(dat) == "status"] <- "group"

# reverse code eve_pleasant into 'event related stress'
dat$stress <- dat$eve_pleasant * -1
dat$eve_pleasant <- NULL

# split dataset by each day within each subject
z <- split(dat, list(dat$subjno, dat$dayno), drop = TRUE)

#??????
retnas <- 4


#########################NA(t-1) baseline######################################


# for each day within each subject, do the following:

dat2 <- lapply(z, function(x) {
  
  # check if some stressful event happened during the day (stress >= 1)
  if (isTRUE(any(x$stress >= 1))) {
    # if so, at which beep did the first stressful event occur? (called b.s = *b*eep of the *s*ressful event)
    b.s <- which(x$stress >= 1)[1]
    # how stressful was this stressful event?
    stress.b.s <- x$stress[b.s]
    # what was the level of negative affect when the stressful event occurred?
    negaff.b.s <- x$negaff[b.s]
    # set t.10 to the beeptime of the last beep of the day
    beep.end = length(x$beepno)
    t.10 <- x$beeptime[beep.end]
    # if the first stressful event of the day occurred on the first beep, skip (since we cannot assess 'recovery' then)
    if (b.s == 1)
      return(c(id=x$subjno[1], eventtype=1, x$group[1], x$sex[1], x$age[1], b.s, stress.b.s, negaff.b.s, NA, NA, NA, t.10, rep(NA,retnas),
               x$mood_worry[b.s],
               x$mood_stressed[b.s],
               x$mood_anxious[b.s],
               x$mood_irritated[b.s],
               x$mood_down[b.s],
               x$mood_tense[b.s],
               x$mood_restless[b.s],rep(NA,12)))
    # set b.b to the *b*eep number right *b*efore the stressful event and t.b to the corresponding time
    b.b <- b.s - 1
    t.b <- x$beeptime[b.b]
    # what was the level of negative affect at the beep right before the stressful event occurred?
    negaff.b.b <- x$negaff[b.b]
    # if this negative affect value is missing, skip (since we cannot assess 'recovery' then)
    if (is.na(negaff.b.b))
      return(c(id=x$subjno[1], eventtype=2, x$group[1], x$sex[1], x$age[1], b.s, stress.b.s, negaff.b.s, NA, t.b, NA, t.10, rep(NA,retnas),
               x$mood_worry[b.s],
               x$mood_stressed[b.s],
               x$mood_anxious[b.s],
               x$mood_irritated[b.s],
               x$mood_down[b.s],
               x$mood_tense[b.s],
               x$mood_restless[b.s],rep(NA,12)))
    # if negative affect is missing for all beeps from b.s to 10, skip (since we cannot assess 'recovery' then)
    if (all(is.na(x$negaff[b.s:beep.end])))
      return(c(id=x$subjno[1], eventtype=3, x$group[1], x$sex[1], x$age[1], b.s, stress.b.s, NA, negaff.b.b, t.b, NA, t.10, rep(NA,retnas),
               x$mood_worry[b.s],
               x$mood_stressed[b.s],
               x$mood_anxious[b.s],
               x$mood_irritated[b.s],
               x$mood_down[b.s],
               x$mood_tense[b.s],
               x$mood_restless[b.s],rep(NA,6),
               x$mood_worry[b.b],
               x$mood_stressed[b.b],
               x$mood_anxious[b.b],
               x$mood_irritated[b.b],
               x$mood_down[b.b],
               x$mood_tense[b.b],
               x$mood_restless[b.b]))
    # at which beep was the negative affect level less than or equal to the one before the stressful event?
    # called b.r = *b*eep where the person has *r*ecovered from the stressful event
    # note: b.r can be equal to b.s and b.r will be missing if recovery did not occur
    b.r <- which((x$negaff <= negaff.b.b) & (1:beep.end >= b.s))[1] ## if stressful event occurs
    if (is.na(b.r)) {
      # if recovery did not occur, then the outcome is right-censored and the follow-up time is the
      # time at the last beep where negaff was not missing minus the time at beep b.b
      return(c(id=x$subjno[1], eventtype=4, x$group[1], x$sex[1], x$age[1], b.s, stress.b.s, negaff.b.s, negaff.b.b, t.b, NA, t.10,
               status = 0,
               time1 = x$beeptime[tail(which(!is.na(x$negaff)), 1)] - t.b,
               time2 = NA,
               again = isTRUE(any(x$stress[(b.s+1):beep.end] >= 1)),
               x$mood_worry[b.s],
               x$mood_stressed[b.s],
               x$mood_anxious[b.s],
               x$mood_irritated[b.s],
               x$mood_down[b.s],
               x$mood_tense[b.s],
               x$mood_restless[b.s],rep(NA,6),
               x$mood_worry[b.b],
               x$mood_stressed[b.b],
               x$mood_anxious[b.b],
               x$mood_irritated[b.b],
               x$mood_down[b.b],
               x$mood_down[b.b],
               x$mood_restless[b.b]))
    } else {
      # if recovery did occur, then this occurred between b.r and the first preceding beep with a non-missing negaff value
      return(c(id=x$subjno[1], eventtype=5, x$group[1], x$sex[1], x$age[1], b.s, stress.b.s, negaff.b.s, negaff.b.b, t.b, x$beeptime[b.r], t.10,
               status = 3,
               time1 = tail(x$beeptime[which(!is.na(x$negaff) & x$beepno >= b.b & x$beepno < b.r)], 1) - t.b,
               time2 = x$beeptime[b.r] - t.b,
               if (b.r==b.s){again = FALSE}
               else{again = isTRUE(any(x$stress[(b.s+1):b.r] >= 1))},
               x$mood_worry[b.s],
               x$mood_stressed[b.s],
               x$mood_anxious[b.s],
               x$mood_irritated[b.s],
               x$mood_down[b.s],
               x$mood_tense[b.s],
               x$mood_restless[b.s],
               x$mood_worry[b.r],
               x$mood_stressed[b.r],
               x$mood_anxious[b.r],
               x$mood_irritated[b.r],
               x$mood_down[b.r],
               x$mood_tense[b.r],
               x$mood_restless[b.r],
               x$mood_worry[b.b],
               x$mood_stressed[b.b],
               x$mood_anxious[b.b],
               x$mood_irritated[b.b],
               x$mood_down[b.b],
               x$mood_tense[b.b],
               x$mood_restless[b.b]))
    }
  } else {
    # if no stressful event happened during the day, just return the subject number, eventtype=6, and the status
    return(c(id=x$subjno[1], eventtype=6, x$group[1], x$sex[1], x$age[1], NA, NA, NA, NA, NA, NA, NA, rep(NA,retnas),rep(NA,18)))
  }
  
})

# combine list elements into a data frame
dat2 <- do.call(rbind, dat2)
dat2 <- data.frame(dat2)
names(dat2) <- c("id", "eventtype", "group", "sex", "age", "b.s", "stress.b.s", "negaff.b.s", "negaff.b.b", "t.b", "t.r", "t.10", "status", "time1", "time2", "again",
                 "mood_worry_b.s","mood_stressed_b.s","mood_anxious_b.s","mood_irritated_b.s","mood_down_b.s","mood_tense_b.s","mood_restless_b.s",
                 "mood_worry_b.r","mood_stressed_b.r","mood_anxious_b.r","mood_irritated_b.r","mood_down_b.r","mood_tense_b.r","mood_restless_b.r",
                 "mood_worry_b.b","mood_stressed_b.b","mood_anxious_b.b","mood_irritated_b.b","mood_down_b.b","mood_tense_b.b","mood_restless_b.b")
rownames(dat2) <- 1:nrow(dat2)
head(dat2, 20)


table(dat2$eventtype)

dat2 <- dat2[dat2$eventtype %in% 4:5,]

# copy dat2 into dat3
dat3 <- dat2

# recode status=3 into status=1
dat3$status[dat3$status == 3] <- 1

# for status=1, use time2 as the amount of time it took for recovery to occur (i.e., ignore the interval issue and use the maximum from the interval as the follow-up time)
dat3$time1[dat3$status == 1] <- dat3$time2[dat3$status == 1]

dat2$time1 <- dat2$time1 + 1
dat2$time2 <- dat2$time2 + 1

smoothSEcurve = function(yy, xx) {
  # use after a call to "plot"
  # fit a lowess curve and 95% confidence interval curve
  # make list of x values
  xx.list = min(xx) + ((0:100)/100)*(max(xx) - min(xx))
  # Then fit loess function through the points (xx, yy)
  # at the listed values
  yy.xx = predict(loess(yy ~ xx), se=T,
                  newdata=data.frame(xx=xx.list))
  lines(yy.xx$fit ~ xx.list, lwd=2)
  lines(yy.xx$fit -
          qt(0.975, yy.xx$df)*yy.xx$se.fit ~ xx.list, lty=2)
  lines(yy.xx$fit +
          qt(0.975, yy.xx$df)*yy.xx$se.fit ~ xx.list, lty=2)
}

surInt = with(dat2,Surv(time1, time2,type="interval2"))
dim(surInt)

dat2 = merge(dat2, SCL, by='id')

fit.survreg = survreg(Surv(time1, time2, type="interval2") ~ sex + age + stress.b.s + again + sum_score + cluster(id), dist="weibull", data=dat2)
summary(fit.survreg)

AIC(fit.survreg)

weib.coef.all = fit.survreg$coef
weib.coef = weib.coef.all[2:6]

#hazard ratio meaning
exp(weib.coef)

SCL.again.1.sex.0 = mean(dat2$sum_score[dat2$again==1 & dat2$sex==0],na.rm=TRUE)
SCL.again.0.sex.0 = mean(dat2$sum_score[dat2$again==0 & dat2$sex==0],na.rm=TRUE)
SCL.again.1.sex.1 = mean(dat2$sum_score[dat2$again==1 & dat2$sex==1],na.rm=TRUE)
SCL.again.0.sex.1 = mean(dat2$sum_score[dat2$again==0 & dat2$sex==1],na.rm=TRUE)

SCL.again.1.sex.0.Q1 = quantile(dat2$sum_score[dat2$again==1 & dat2$sex==0],probs=0.05,na.rm=TRUE)
SCL.again.1.sex.0.Q3 = quantile(dat2$sum_score[dat2$again==1 & dat2$sex==0],probs=0.95,na.rm=TRUE)

SCL.again.0.sex.0.Q1 = quantile(dat2$sum_score[dat2$again==0 & dat2$sex==0],probs=0.05,na.rm=TRUE)
SCL.again.0.sex.0.Q3 = quantile(dat2$sum_score[dat2$again==0 & dat2$sex==0],probs=0.95,na.rm=TRUE)

SCL.again.1.sex.1.Q1 = quantile(dat2$sum_score[dat2$again==1 & dat2$sex==1],probs=0.05,na.rm=TRUE)
SCL.again.1.sex.1.Q3 = quantile(dat2$sum_score[dat2$again==1 & dat2$sex==1],probs=0.95,na.rm=TRUE)

SCL.again.0.sex.1.Q1 = quantile(dat2$sum_score[dat2$again==0 & dat2$sex==1],probs=0.05,na.rm=TRUE)
SCL.again.0.sex.1.Q3 = quantile(dat2$sum_score[dat2$again==0 & dat2$sex==1],probs=0.95,na.rm=TRUE)

stress.b.s.again.1.sex.0 = mean(dat2$stress.b.s[dat2$again==1 & dat2$sex==0],na.rm=TRUE)
stress.b.s.again.1.sex.1 = mean(dat2$stress.b.s[dat2$again==1 & dat2$sex==1],na.rm=TRUE)
stress.b.s.again.0.sex.0 = mean(dat2$stress.b.s[dat2$again==0 & dat2$sex==0],na.rm=TRUE)
stress.b.s.again.0.sex.1 = mean(dat2$stress.b.s[dat2$again==0 & dat2$sex==1],na.rm=TRUE)

age.mean.again.1.sex.0 = mean(dat2$age[dat2$again==1 & dat2$sex==0])
age.mean.again.1.sex.1 = mean(dat2$age[dat2$again==1 & dat2$sex==1])
age.mean.again.0.sex.0 = mean(dat2$age[dat2$again==0 & dat2$sex==0])
age.mean.again.0.sex.1 = mean(dat2$age[dat2$again==0 & dat2$sex==1])

# Get percentiles and confidence intervals
# Percentiles for again=1, sex = 0
q1 = predict(fit.survreg,
             newdata=list(again=1,sex=0,age=age.mean.again.1.sex.0,sum_score=SCL.again.1.sex.0,
                          stress.b.s = stress.b.s.again.1.sex.0),
             type="quantile", p=c(.1, .5, .9), se.fit=T)

ci1 = cbind(q1$fit, q1$fit - 1.96*q1$se.fit, q1$fit + 1.96*q1$se.fit)
dimnames(ci1) = list(c(.1, .5, .9), c("Estimate", "Lower ci", "Upper ci"))
round(ci1)

# again = 1,sex=1
q1 = predict(fit.survreg,
             newdata=list(again=1,sex=1,age=age.mean.again.1.sex.1,sum_score=SCL.again.1.sex.1,
                          stress.b.s = stress.b.s.again.1.sex.1),
             type="quantile", p=c(.1, .5, .9), se.fit=T)

ci1 = cbind(q1$fit, q1$fit - 1.96*q1$se.fit, q1$fit + 1.96*q1$se.fit)
dimnames(ci1) = list(c(.1, .5, .9), c("Estimate", "Lower ci", "Upper ci"))
round(ci1)

# again = 0,sex=0
q1 = predict(fit.survreg,
             newdata=list(again=0,sex=0,age=age.mean.again.0.sex.0,sum_score=SCL.again.0.sex.0,
                          stress.b.s = stress.b.s.again.0.sex.0),
             type="quantile", p=c(.1, .5, .9), se.fit=T)

ci1 = cbind(q1$fit, q1$fit - 1.96*q1$se.fit, q1$fit + 1.96*q1$se.fit)
dimnames(ci1) = list(c(.1, .5, .9), c("Estimate", "Lower ci", "Upper ci"))
round(ci1)

# again = 0,sex=1
q1 = predict(fit.survreg,
             newdata=list(again=0,sex=1,age=age.mean.again.0.sex.1,sum_score=SCL.again.0.sex.1,
                          stress.b.s = stress.b.s.again.0.sex.1),
             type="quantile", p=c(.1, .5, .9), se.fit=T)

ci1 = cbind(q1$fit, q1$fit - 1.96*q1$se.fit, q1$fit + 1.96*q1$se.fit)
dimnames(ci1) = list(c(.1, .5, .9), c("Estimate", "Lower ci", "Upper ci"))
round(ci1)

par(mfrow = c(2, 2))

#again=1,sex=1
plot(predict(fit.survreg, newdata=list(again=1,sex=1,age=age.mean.again.1.sex.1,sum_score=SCL.again.1.sex.1,stress.b.s = stress.b.s.again.1.sex.1),
             type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),
     col="black",type='l',xlab="Minutes", ylab="Not Recovery Probability",main='Female With Cumulative Stressor',xlim=c(0,3000))
lines(predict(fit.survreg, newdata=list(again=1,sex=1,age=age.mean.again.1.sex.1,sum_score=SCL.again.1.sex.1.Q1,stress.b.s = stress.b.s.again.1.sex.1),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")
lines(predict(fit.survreg, newdata=list(again=1,sex=1,age=age.mean.again.1.sex.1,sum_score=SCL.again.1.sex.1.Q3,stress.b.s = stress.b.s.again.1.sex.1),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")
legend(x = "topright", legend=c("Mean", "10%","90%"),
       col=c("black", "blue","red"), lty=1, cex=0.8)


#again=1,sex=0
plot(predict(fit.survreg, newdata=list(again=1,sex=0,age=age.mean.again.1.sex.0,sum_score=SCL.again.1.sex.0,stress.b.s = stress.b.s.again.1.sex.0),
             type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),
     col="black",type='l',xlab="Minutes", ylab="Not Recovery Probability",main='Male With Cumulative Stressor',xlim=c(0,3000))
lines(predict(fit.survreg, newdata=list(again=1,sex=0,age=age.mean.again.1.sex.0,sum_score=SCL.again.1.sex.0.Q1,stress.b.s = stress.b.s.again.1.sex.0),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")
lines(predict(fit.survreg, newdata=list(again=1,sex=0,age=age.mean.again.1.sex.0,sum_score=SCL.again.1.sex.0.Q3,stress.b.s = stress.b.s.again.1.sex.0),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")
legend(x = "topright", legend=c("Mean", "10%","90%"),
       col=c("black", "blue","red"), lty=1, cex=0.8)

#again=0,sex=1
plot(predict(fit.survreg, newdata=list(again=0,sex=1,age=age.mean.again.0.sex.1,sum_score=SCL.again.0.sex.1,stress.b.s = stress.b.s.again.0.sex.1),
             type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),
     col="black",type='l',xlab="Minutes", ylab="Not Recovery Probability",main='Female Without Cumulative Stressor',xlim=c(0,3000))
lines(predict(fit.survreg, newdata=list(again=0,sex=1,age=age.mean.again.0.sex.1,sum_score=SCL.again.0.sex.1.Q1,stress.b.s = stress.b.s.again.0.sex.1),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")
lines(predict(fit.survreg, newdata=list(again=0,sex=1,age=age.mean.again.0.sex.1,sum_score=SCL.again.0.sex.1.Q3,stress.b.s = stress.b.s.again.0.sex.1),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")
legend(x = "topright", legend=c("Mean", "10%","90%"),
       col=c("black", "blue","red"), lty=1, cex=0.8)

#again=0,sex=0
plot(predict(fit.survreg, newdata=list(again=0,sex=0,age=age.mean.again.0.sex.0,sum_score=SCL.again.0.sex.0,stress.b.s = stress.b.s.again.0.sex.0),
             type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),
     col="black",type='l',xlab="Minutes", ylab="Not Recovery Probability",main='Male Without Cumulative Stressor',xlim=c(0,3000))
lines(predict(fit.survreg, newdata=list(again=0,sex=0,age=age.mean.again.0.sex.0,sum_score=SCL.again.0.sex.0.Q1,stress.b.s = stress.b.s.again.0.sex.0),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="blue")
lines(predict(fit.survreg, newdata=list(again=0,sex=0,age=age.mean.again.0.sex.0,sum_score=SCL.again.0.sex.0.Q3,stress.b.s = stress.b.s.again.0.sex.0),
              type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col="red")
legend(x = "topright", legend=c("Mean", "10%","90%"),
       col=c("black", "blue","red"), lty=1, cex=0.8)







