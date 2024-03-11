
#time depedent data construction : transplat

library(survival)

head(jasa)

jasa$subject<-1:nrow(jasa)

jasa2<-subset(jasa,select=c(subject,futime,fustat,tx.date,wait.time,mscore))


num.obs<-100 #1000
jasa3<-subset(jasa2,subject<num.obs)
#add 0.5 to all time data to have longer futime than the wait.time
jasa3$futime<-0.5+jasa3$futime
#wait.time: transplant 가 없었던 경우 NA,있었던 경우는 시점 기록



tdata<-with(jasa3,data.frame(subject=subject,futime=futime,
txtime=wait.time,fustat=fustat))
head(tdata,10)





sdata<-tmerge(jasa3,tdata,id=subject,death=event(futime,fustat),trt=tdc(txtime))
head(sdata,10)

final.data<-subset(sdata,select=-c(subject,futime,fustat,tx.date,wait.time))
head(final.data,10)

summary(fit <- coxph(Surv(tstart, tstop, death==1) ~ trt, data=final.data))
anova(fit)
