library(survival);library(survivalROC);library(data.table);library(moonBook)
setwd("C:/Users/chlwo/OneDrive/바탕 화면/생존 과제")
###########################################################
dat1<-fread("breast.csv")
dat1
#N=686
summary(dat1)

#예후가 좋지 않은 것이 coding 숫자가 큼
#treat 변수 재코딩
dat1[treat==1,tr:=0] 
dat1[treat==0,tr:=1]

#size categorization
dat1[size<=20,c_size:=1]
dat1[21<=size&size<=50,c_size:=2]
dat1[50<size,c_size:=3]
table(dat1$c_size)

#nodes categorization
dat1[nodes<4,c_nodes:=1]
dat1[4<=nodes&nodes<10,c_nodes:=2]
dat1[10<=nodes,c_nodes:=3]
table(dat1$c_nodes)

#grade
dat1[grade==1,grade1:=1]
dat1[grade1%in%NA,grade1:=0]
dat1[grade==2,grade2:=1]
dat1[grade2%in%NA,grade2:=0]
dat1[grade==3,grade3:=1]
dat1[grade3%in%NA,grade3:=0]

#size+nodes new level variable
dat1[c_size==1&c_nodes==1,stage:=1]
dat1[c_size==1&c_nodes==2,stage:=3]
dat1[c_size==1&c_nodes==3,stage:=4]
dat1[c_size==2&c_nodes==1,stage:=2]
dat1[c_size==2&c_nodes==2,stage:=3]
dat1[c_size==2&c_nodes==3,stage:=4]
dat1[c_size==3&c_nodes==1,stage:=3]
dat1[c_size==3&c_nodes==2,stage:=3]
dat1[c_size==3&c_nodes==3,stage:=4]

dat1[c_size==1&c_nodes==1,stage2A:=1]
dat1[stage2A%in%NA, stage2A:=0]
dat1[c_size==2&c_nodes==1,stage2B:=1]
dat1[stage2B%in%NA, stage2B:=0]
dat1[c_size==1&c_nodes==2,stage3A:=1]
dat1[c_size==2&c_nodes==2,stage3A:=1]
dat1[c_size==3&c_nodes==1,stage3A:=1]
dat1[c_size==3&c_nodes==2,stage3A:=1]
dat1[stage3A%in%NA, stage3A:=0]
dat1[c_size==1&c_nodes==3,stage3C:=1]
dat1[c_size==2&c_nodes==3,stage3C:=1]
dat1[c_size==3&c_nodes==3,stage3C:=1]
dat1[stage3C%in%NA,stage3C:=0]

#prog categorization
dat1[prog<20,c_prog:=1]
dat1[prog>=20,c_prog:=2]
#oest categorization 
dat1[oest<20,c_oest:=1]
dat1[oest>=20,c_oest:=2]

colnames(dat1)
dat2 <- dat1[,.(id,status,time,tr,age,men,
                grade1,grade2,grade3,c_prog,c_oest,
                stage,stage2A,stage2B,stage3A,stage3C)]

###########################################################
set.seed(3)
ind<-sample(1:dim(dat2)[1],replace=F)

dat3<-dat2[ind,]

(nn.train<-round(dim(dat2)[1]*0.8)) #549

#train data set : 80%
train.d<-temp<-dat3[1:nn.train,]
train.d$cohort.test<-0

#test data set : 20%
test.d<-dat3[(nn.train+1):(dim(dat2)[1]),]
nn.test<-nrow(test.d) #137
test.d$cohort.test<-1

#합치기
dat.all<-rbind(train.d,test.d)
fwrite(dat.all,'data_all_train_test.csv')

###########################################################
dat.all<-read.csv("data_all_train_test.csv")

test.d<-subset(dat.all,cohort.test==1)
train.d<-subset(dat.all,cohort.test==0)

table(train.d$stage,train.d$status)

#univariate model for new level variable
f0 <- coxph(Surv(time,status)~factor(stage),method="breslow", data=train.d)
summary(f0)

#multivariable model
f1 <- coxph(Surv(time,status)~factor(tr)+age+factor(men)
            +factor(grade1)+factor(grade2)+factor(grade3)
            +factor(c_prog)+factor(c_oest)
            +factor(stage2A)+factor(stage2B)+factor(stage3A)+factor(stage3C)
             , method="breslow", data=train.d)
summary(f1)



new.data<-data.frame(tr=c(0),age=c(0),men=c(1),
                     grade1=c(0),grade2=c(0),grade3=c(0),
                     c_prog=c(1),c_oest=c(1),
                     stage2A=c(0),stage2B=c(0),stage3A=c(0),stage3C=c(0))

sfit<-survfit(f1, newdata=new.data)
survest <- stepfun(sfit$time, c(1, sfit$surv))

#Baseline  3, 5 and 10-year survival : S_0(t)
(est.10y<-survest(10*365)) 
(est.5y<-survest(5*365))   
(est.3y<-survest(3*365))   

est.10y<-0.0411
est.5y<-0.1294
est.3y<-0.3348


total.points<-(-35:11)
B<-0.044
age.ref<-38
coefs.age<--0.002675

risk.score<-coefs.age*age.ref+B*(total.points)

(p.hat.5year<-1-(est.5y^(exp(risk.score))))
(p.hat.3year<-1-(est.3y^(exp(risk.score))))
(p.hat.10year<-1-(est.10y^(exp(risk.score))))


tab<-cbind(total.points,100*p.hat.10year,100*p.hat.5year,100*p.hat.3year)
write.csv(tab,'total.point.risk.10.5.3y.csv')


plot(total.points,p.hat.10year,type='n',ylab='Calculated Risk',
     xlab='Total Points',ylim = c(0,1),xlim=c(-35,10))
lines(total.points,p.hat.3year,lty=1,col=1,lwd=2)
lines(total.points,p.hat.5year,lty=1,col=2,lwd=2)
lines(total.points,p.hat.10year,lty=1,col=3,lwd=2)
legend('topleft',c('3-year','5-year','10-year'),bty='n',col=1:3,lwd=2)
grid()


#Table for the Estimate of Risk
dt<-train.d
te<-dt$age

p1<-ifelse(te<=45,0,ifelse(te<=60,-1,-2))
p2<-ifelse(dt$tr==0,0,11)
p3<-ifelse(dt$men==1,0,4)
p4<-ifelse(dt$grade1==0,0,-5)
p5<-ifelse(dt$grade2==0,0,1)
p6<-ifelse(dt$grade3==0,0,0)
p7<-ifelse(dt$c_prog==0,0,-15)
p8<-ifelse(dt$c_oest==0,0,-1)
p9<-ifelse(dt$stage2A==0,0,-35)
p10<-ifelse(dt$stage2B==0,0,-27)
p11<-ifelse(dt$stage3A==0,0,-15)
p12<-ifelse(dt$stage3C==0,0,0)

dt$total.points<-p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12

dt$risk.score<-coefs.age*age.ref+B*(dt$total.points)


dt$p.hat.5year<-1-(est.5y^(exp(dt$risk.score)))
dt$p.hat.3year<-1-(est.3y^(exp(dt$risk.score)))
dt$p.hat.10year<-1-(est.10y^(exp(dt$risk.score)))

dt2 <- subset(dt,select=c(total.points,risk.score,p.hat.10year,p.hat.5year,p.hat.3year))

dt3=unique(dt2,by="total.points")
dt4=arrange(dt3,total.points)

#################
#ROC curve
fit.neg.3<- survivalROC(Stime=dt$time/365,status=dt$status,
                        marker =dt$p.hat.3year,predict.time = 3,span= 0.25*nrow(dt)^(-0.20));
(re.3.train<-fit.neg.3$AUC)

fit.neg.5<- survivalROC(Stime=dt$time/365,status=dt$status,
                        marker =dt$p.hat.5year,predict.time = 5,span= 0.25*nrow(dt)^(-0.20));
(re.5.train<-fit.neg.5$AUC)

fit.neg.10<- survivalROC(Stime=dt$time/365,status=dt$status,
                         marker =dt$p.hat.10year,predict.time = 10,span= 0.25*nrow(dt)^(-0.20));
(re.10.train<-fit.neg.10$AUC)

auroc.fun<-function(temp){
  fit.neg.5<- survivalROC(Stime=temp$time/365,status=temp$status,
                          marker =temp$p.hat.5year,predict.time = 5,span= 0.25*nrow(temp)^(-0.20));
  re.5.train<-fit.neg.5$AUC
  fit.neg.3<- survivalROC(Stime=temp$time/365,status=temp$status,
                          marker =temp$p.hat.3year,predict.time = 3,span= 0.25*nrow(temp)^(-0.20));
  re.3.train<-fit.neg.3$AUC
  fit.neg.10<- survivalROC(Stime=temp$time/365,status=temp$status,
                           marker =temp$p.hat.10year,predict.time = 10,span= 0.25*nrow(temp)^(-0.20));
  re.10.train<-fit.neg.10$AUC
  re<-c(re.3.train,re.5.train,re.10.train)
  return(re)
}

auroc.boot<-function(){
  ind<-sample(nrow(dt),replace=TRUE)
  re2<-auroc.fun(dt[ind,])
  return(re2)
}

auroc.boot.fun.res<-t(replicate(500,auroc.boot()))

auroc.boot.fun.res<-data.frame(auroc.boot.fun.res)
names(auroc.boot.fun.res)<-c('3y','5y','10y')
write.csv(auroc.boot.fun.res,'auroc.boot.fun.res.train.csv')


lb.ub.auc.train<-rbind(quantile(auroc.boot.fun.res[,1],c(0.025,0.975)),
                       quantile(auroc.boot.fun.res[,2],c(0.025,0.975)),
                       quantile(auroc.boot.fun.res[,3],c(0.025,0.975)))


#ROC plot for scoring system

plot(fit.neg.5$FP,fit.neg.5$TP,xlab="100-Specificity (%)",
     ylab="Sensitivity (%)",main="",
     xaxt='n',yaxt='n',type='n')


lines(fit.neg.3$FP,fit.neg.3$TP,col=1,lwd=2)
lines(fit.neg.5$FP,fit.neg.5$TP,col=2,lwd=2)
lines(fit.neg.10$FP,fit.neg.10$TP,col=3,lwd=2)



axis(1, at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,20,40,60,80,100))
axis(2, at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,20,40,60,80,100))

lines( seq(0,1,length.out=100), seq(0,1,length.out=100),col=1,lty=2)

legend(0.48,0.48,paste('3-year risk'),bty='n',col=1,lwd=2,box.col='white')
legend(0.48,0.41,paste('AUROC=',format(round(re.3.train,3), nsmall = 3),',',
                       '95% CI:',format(round(lb.ub.auc.train[1,1],2),nsmall=2),'-',
                       format(round(lb.ub.auc.train[1,2],2),nsmall=2)),bty='n',col=2,box.col='white')

legend(0.48,0.34,paste('5-year risk'),bty='n',col=2,lwd=2,box.col='white')
legend(0.48,0.27,paste('AUROC=',format(round(re.5.train,3), nsmall = 3),',',
                       '95% CI:',format(round(lb.ub.auc.train[2,1],2),nsmall=2),'-',
                       format(round(lb.ub.auc.train[2,2],2),nsmall=2)),bty='n',col=2,box.col='white')

legend(0.48,0.20,paste('10-year risk'),bty='n',col=3,lwd=2,box.col='white')
legend(0.48,0.13,paste('AUROC=',format(round(re.10.train,3), nsmall = 3),',',
                       '95% CI:',format(round(lb.ub.auc.train[3,1],2),nsmall=2),'-',
                       format(round(lb.ub.auc.train[3,2],2),nsmall=2)),bty='n',col=2,box.col='white')



##########################
#calibration plot

d<-dt

#Time points we are interested in calibration 

estinc=d$p.hat.3year
summary(estinc)
d$dec=ifelse(estinc<0.16281,1,
             ifelse(estinc<0.24113,2,ifelse(estinc<0.34847,3,4)))
table(d$dec,d$status)

dt$s.cut.3y<-dt$group.3y<-d$dec

mean.sr.pred<-aggregate(dt$p.hat.3year,list(dt$group.3y),mean)[,2]

kmfit.fun<-function(jj,time.points){
  kmfit <- survfit(Surv(time/365,status)~1,data=subset(dt,group.3y==jj))
  survest.pt <- stepfun(kmfit$time, c(1, kmfit$surv))
  survest.lb <- stepfun(kmfit$time, c(1, kmfit$lower))
  survest.ub <- stepfun(kmfit$time, c(1, kmfit$upper))
  pt<-1-survest.pt(time.points)
  ub<-1-survest.lb(time.points)
  lb<-1-survest.ub(time.points)
  re<-c(pt,lb,ub)
  return(re)
}

time.pts<-3
kmfit.3y<-rbind(kmfit.fun(1,time.pts),kmfit.fun(2,time.pts),
                kmfit.fun(3,time.pts),
                kmfit.fun(4,time.pts))

kmfit.3y<-data.frame(kmfit.3y)
(names(kmfit.3y)<-c('pt','lb','ub'))
cbind(mean.sr.pred,kmfit.3y)
mean.sr.pred.3y<-mean.sr.pred
#
estinc=d$p.hat.5year
summary(estinc)
d$dec=ifelse(estinc<0.2826,1,
             ifelse(estinc<0.4029,2,ifelse(estinc<0.5510,3,4)))
table(d$dec,d$status)

dt$s.cut.5y<-dt$group.5y<-d$dec

mean.sr.pred<-aggregate(dt$p.hat.5year,list(dt$group.5y),mean)[,2]

kmfit.fun<-function(jj,time.points){
  kmfit <- survfit(Surv(time/365,status)~1,
                   data=subset(dt,group.5y==jj))
  survest.pt <- stepfun(kmfit$time, c(1, kmfit$surv))
  survest.lb <- stepfun(kmfit$time, c(1, kmfit$lower))
  survest.ub <- stepfun(kmfit$time, c(1, kmfit$upper))
  pt<-1-survest.pt(time.points)
  ub<-1-survest.lb(time.points)
  lb<-1-survest.ub(time.points)
  re<-c(pt,lb,ub)
  return(re)
}
time.pts<-5
kmfit.5y<-rbind(kmfit.fun(1,time.pts),kmfit.fun(2,time.pts),
                kmfit.fun(3,time.pts),kmfit.fun(4,time.pts))

kmfit.5y<-data.frame(kmfit.5y)
(names(kmfit.5y)<-c('pt','lb','ub'))
cbind(mean.sr.pred,kmfit.5y)

mean.sr.pred.5y<-mean.sr.pred


#
estinc=d$p.hat.10year
summary(estinc)
d$dec=ifelse(estinc<0.4045,1,
             ifelse(estinc<0.5529,2,ifelse(estinc<0.7134,3,4)))
table(d$dec,d$status)

dt$s.cut.10y<-dt$group.10y<-d$dec

mean.sr.pred<-aggregate(dt$p.hat.10year,list(dt$group.10y),mean)[,2]
#mean.sr.pred.tank[,2]<-mean.sr.pred


kmfit.fun<-function(jj,time.points){
  kmfit <- survfit(Surv(time/365,status)~1,
                   data=subset(dt,group.10y==jj))
  survest.pt <- stepfun(kmfit$time, c(1, kmfit$surv))
  survest.lb <- stepfun(kmfit$time, c(1, kmfit$lower))
  survest.ub <- stepfun(kmfit$time, c(1, kmfit$upper))
  pt<-1-survest.pt(time.points)
  ub<-1-survest.lb(time.points)
  lb<-1-survest.ub(time.points)
  re<-c(pt,lb,ub)
  return(re)
}
time.pts<-10
kmfit.10y<-rbind(kmfit.fun(1,time.pts),kmfit.fun(2,time.pts),
                 kmfit.fun(3,time.pts),kmfit.fun(4,time.pts))

kmfit.10y<-data.frame(kmfit.10y)
(names(kmfit.10y)<-c('pt','lb','ub'))
cbind(mean.sr.pred,kmfit.10y)
mean.sr.pred.10y<-mean.sr.pred

#kmfit.3y,kmfit.5y,kmfit.10y
#mean.sr.pred.3y,mean.sr.pred.5y,mean.sr.pred.10y


mean.sr.pred.tank<-cbind(mean.sr.pred.3y,mean.sr.pred.5y,mean.sr.pred.10y)

(mean.sr.pred.tank<-data.frame(mean.sr.pred.tank))
(names(mean.sr.pred.tank)<-c('3y','5y','10y'))

options(scipen=5)
plot(c(-4,0),c(-4,0),xlab='Mean risk-score predicted risk',
     ylab='KM observed risk',xlim=c(-1.5,0), ylim=c(-1.5,0),type='n',axes=F)
axis(1, at=c(-4,-3,-2,-1,0), labels=c(0.0001,0.001,0.01,0.1,1))
axis(2, at=c(-4,-3,-2,-1,0), labels=c(0.0001,0.001,0.01,0.1,1))

lines( seq(-4,0,length.out=100), seq(-4,0,length.out=100),col=1,lty=2)

points(log10(mean.sr.pred.tank[,1]),log10(kmfit.3y[,1]),col=1,pch=16)
lines(log10(mean.sr.pred.tank[,1]),log10(kmfit.3y[,1]),col=1)

points(log10(mean.sr.pred.tank[,2]),log10(kmfit.5y[,1]),col=2,pch=16)
lines(log10(mean.sr.pred.tank[,2]),log10(kmfit.5y[,1]),col=2)

points(log10(mean.sr.pred.tank[,3]),log10(kmfit.10y[,1]),col=3,pch=16)
lines(log10(mean.sr.pred.tank[,3]),log10(kmfit.10y[,1]),col=3)

legend("bottomright",c('3-years','5-years','10-years'),
       bty='n',col=c(1,2,3),lwd=1,ncol=1,pch=16)

############################
#test data  validation

dt<-test.d
te<-dt$age
p1<-ifelse(te<=45,0,ifelse(te<=60,-1,-2))
p2<-ifelse(dt$tr==0,0,11)
p3<-ifelse(dt$men==1,0,4)
p4<-ifelse(dt$grade1==0,0,-5)
p5<-ifelse(dt$grade2==0,0,1)
p6<-ifelse(dt$grade3==0,0,0)
p7<-ifelse(dt$c_prog==0,0,-15)
p8<-ifelse(dt$c_oest==0,0,-1)
p9<-ifelse(dt$stage2A==0,0,-35)
p10<-ifelse(dt$stage2B==0,0,-27)
p11<-ifelse(dt$stage3A==0,0,-15)
p12<-ifelse(dt$stage3C==0,0,0)

dt$total.points<-p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12
dt$risk.score<-coefs.age*age.ref+B*(dt$total.points)


dt$p.hat.5year<-1-(est.5y^(exp(dt$risk.score)))
dt$p.hat.3year<-1-(est.3y^(exp(dt$risk.score)))
dt$p.hat.10year<-1-(est.10y^(exp(dt$risk.score)))


#################################################
#roc curve for the test data

fit.neg.3<- survivalROC(Stime=dt$time/365,status=dt$status,
                        marker =dt$p.hat.3year,predict.time = 3,span= 0.25*nrow(dt)^(-0.20));
(re.3.train<-fit.neg.3$AUC)
fit.neg.5<- survivalROC(Stime=dt$time/365,status=dt$status,
                        marker =dt$p.hat.5year,predict.time = 5,span= 0.25*nrow(dt)^(-0.20));
(re.5.train<-fit.neg.5$AUC)
fit.neg.10<- survivalROC(Stime=dt$time/365,status=dt$status,
                         marker =dt$p.hat.10year,predict.time = 10,span= 0.25*nrow(dt)^(-0.20));
(re.10.train<-fit.neg.10$AUC)


auroc.fun<-function(temp){
  fit.neg.5<- survivalROC(Stime=temp$time/365,status=temp$status,
                          marker =temp$p.hat.5year,predict.time = 5,span= 0.25*nrow(temp)^(-0.20));
  re.5.train<-fit.neg.5$AUC
  fit.neg.3<- survivalROC(Stime=temp$time/365,status=temp$status,
                          marker =temp$p.hat.3year,predict.time = 3,span= 0.25*nrow(temp)^(-0.20));
  re.3.train<-fit.neg.3$AUC
  fit.neg.10<- survivalROC(Stime=temp$time/365,status=temp$status,
                           marker =temp$p.hat.10year,predict.time = 10,span= 0.25*nrow(temp)^(-0.20));
  re.10.train<-fit.neg.10$AUC
  re<-c(re.3.train,re.5.train,re.10.train)
  return(re)
}

auroc.boot<-function(){
  ind<-sample(nrow(dt),replace=TRUE)
  re2<-auroc.fun(dt[ind,])
  return(re2)
}

auroc.boot.fun.res<-t(replicate(500,auroc.boot()))
auroc.boot.fun.res<-data.frame(auroc.boot.fun.res)
names(auroc.boot.fun.res)<-c('3y','5y','10y')
write.csv(auroc.boot.fun.res,'auroc.boot.fun.res.test.csv')

lb.ub.auc.train<-rbind(quantile(auroc.boot.fun.res[,1],c(0.025,0.975)),
                       quantile(auroc.boot.fun.res[,2],c(0.025,0.975)),
                       quantile(auroc.boot.fun.res[,3],c(0.025,0.975)))


#ROC plot for scoring system

plot(fit.neg.5$FP,fit.neg.5$TP,xlab="100-Specificity (%)",
     ylab="Sensitivity (%)",main="",
     xaxt='n',yaxt='n',type='n')
lines(fit.neg.3$FP,fit.neg.3$TP,col=1,lwd=2)
lines(fit.neg.5$FP,fit.neg.5$TP,col=2,lwd=2)
lines(fit.neg.10$FP,fit.neg.10$TP,col=3,lwd=2)

axis(1, at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,20,40,60,80,100))
axis(2, at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,20,40,60,80,100))

lines( seq(0,1,length.out=100), seq(0,1,length.out=100),col=1,lty=2)

legend(0.45,0.48,paste('3-year risk'),bty='n',col=1,lwd=2,box.col='white')
legend(0.45,0.41,paste('AUROC=',format(round(re.3.train,3), nsmall = 3),',',
                       '95% CI:',format(round(lb.ub.auc.train[1,1],2),nsmall=2),'-',
                       format(round(lb.ub.auc.train[1,2],2),nsmall=2)),bty='n',col=2,box.col='white')

legend(0.45,0.34,paste('5-year risk'),bty='n',col=2,lwd=2,box.col='white')
legend(0.45,0.27,paste('AUROC=',format(round(re.5.train,3), nsmall = 3),',',
                       '95% CI:',format(round(lb.ub.auc.train[2,1],2),nsmall=2),'-',
                       format(round(lb.ub.auc.train[2,2],2),nsmall=2)),bty='n',col=2,box.col='white')

legend(0.45,0.20,paste('10-year risk'),bty='n',col=3,lwd=2,box.col='white')
legend(0.45,0.13,paste('AUROC=',format(round(re.10.train,3), nsmall = 3),',',
                       '95% CI:',format(round(lb.ub.auc.train[3,1],2),nsmall=2),'-',
                       format(round(lb.ub.auc.train[3,2],2),nsmall=2)),bty='n',col=2,box.col='white')
##################


############################
#calibration curve

##############################################################
mean.sr.pred.tank<-matrix(NA,nc=3,nr=5)
d<-dt
#the time we are interested in calibration

estinc=d$p.hat.3year
summary(estinc)
d$dec=ifelse(estinc<0.17639,1,
             ifelse(estinc<0.23207,2,ifelse(estinc<0.34847,3,4)))
table(d$dec,d$status)

dt$s.cut.3y<-dt$group.3y<-d$dec


mean.sr.pred<-aggregate(dt$p.hat.3year,list(dt$group.3y),mean)[,2]

kmfit.fun<-function(jj,time.points){
  kmfit <- survfit(Surv(time/365,status)~1,data=subset(dt,group.3y==jj))
  survest.pt <- stepfun(kmfit$time, c(1, kmfit$surv))
  survest.lb <- stepfun(kmfit$time, c(1, kmfit$lower))
  survest.ub <- stepfun(kmfit$time, c(1, kmfit$upper))
  pt<-1-survest.pt(time.points)
  ub<-1-survest.lb(time.points)
  lb<-1-survest.ub(time.points)
  re<-c(pt,lb,ub)
  return(re)
}
time.pts<-3
kmfit.3y<-rbind(kmfit.fun(1,time.pts),kmfit.fun(2,time.pts),
                kmfit.fun(3,time.pts),
                kmfit.fun(4,time.pts))

kmfit.3y<-data.frame(kmfit.3y)
(names(kmfit.3y)<-c('pt','lb','ub'))
cbind(mean.sr.pred,kmfit.3y)

mean.sr.pred.3y<-mean.sr.pred


#

estinc=d$p.hat.5year
summary(estinc)
d$dec=ifelse(estinc<0.3042,1,ifelse(estinc<0.3895,2,
                                   ifelse(estinc<0.5510,3,4)))

table(d$dec,d$status)



dt$s.cut.5y<-dt$group.5y<-d$dec

mean.sr.pred<-aggregate(dt$p.hat.5year,list(dt$group.5y),mean)[,2]
#mean.sr.pred.tank[,2]<-mean.sr.pred


kmfit.fun<-function(jj,time.points){
  kmfit <- survfit(Surv(time/365,status)~1,
                   data=subset(dt,group.5y==jj))
  survest.pt <- stepfun(kmfit$time, c(1, kmfit$surv))
  survest.lb <- stepfun(kmfit$time, c(1, kmfit$lower))
  survest.ub <- stepfun(kmfit$time, c(1, kmfit$upper))
  pt<-1-survest.pt(time.points)
  ub<-1-survest.lb(time.points)
  lb<-1-survest.ub(time.points)
  re<-c(pt,lb,ub)
  return(re)
}
time.pts<-5
kmfit.5y<-rbind(kmfit.fun(1,time.pts),kmfit.fun(2,time.pts),
                kmfit.fun(3,time.pts),kmfit.fun(4,time.pts))

kmfit.5y<-data.frame(kmfit.5y)
(names(kmfit.5y)<-c('pt','lb','ub'))
cbind(mean.sr.pred,kmfit.5y)

mean.sr.pred.5y<-mean.sr.pred

##################

#
estinc=d$p.hat.10year
summary(estinc)
d$dec=ifelse(estinc<0.4322,1,
             ifelse(estinc<0.5371,2,ifelse(estinc<0.7134,3,4)))

table(d$dec,d$status)

dt$s.cut.10y<-dt$group.10y<-d$dec

mean.sr.pred<-aggregate(dt$p.hat.10year,list(dt$group.10y),mean)[,2]
#mean.sr.pred.tank[,2]<-mean.sr.pred


kmfit.fun<-function(jj,time.points){
  kmfit <- survfit(Surv(time/365,status)~1,
                   data=subset(dt,group.10y==jj))
  survest.pt <- stepfun(kmfit$time, c(1, kmfit$surv))
  survest.lb <- stepfun(kmfit$time, c(1, kmfit$lower))
  survest.ub <- stepfun(kmfit$time, c(1, kmfit$upper))
  pt<-1-survest.pt(time.points)
  ub<-1-survest.lb(time.points)
  lb<-1-survest.ub(time.points)
  re<-c(pt,lb,ub)
  return(re)
}
time.pts<-10
kmfit.10y<-rbind(kmfit.fun(1,time.pts),kmfit.fun(2,time.pts),
                 kmfit.fun(3,time.pts),kmfit.fun(4,time.pts))

kmfit.10y<-data.frame(kmfit.10y)
(names(kmfit.10y)<-c('pt','lb','ub'))
cbind(mean.sr.pred,kmfit.10y)

mean.sr.pred.10y<-mean.sr.pred


#kmfit.3y,kmfit.5y,kmfit.10y
#mean.sr.pred.3y,mean.sr.pred.5y,mean.sr.pred.10y


mean.sr.pred.tank<-cbind(mean.sr.pred.3y,c(mean.sr.pred.5y),
                         c(mean.sr.pred.10y))

(mean.sr.pred.tank<-data.frame(mean.sr.pred.tank))
(names(mean.sr.pred.tank)<-c('3y','5y','10y'))

options(scipen=5)
plot(c(-4,0),c(-4,0),xlab='Mean risk-score predicted risk',
     ylab='KM observed risk',type='n',xlim=c(-1.5,0), ylim=c(-1.5,0),axes=F)
axis(1, at=c(-4,-3,-2,-1,0), labels=c(0.0001,0.001,0.01,0.1,1))
axis(2, at=c(-4,-3,-2,-1,0), labels=c(0.0001,0.001,0.01,0.1,1))

lines( seq(-4,0,length.out=100), seq(-4,0,length.out=100),col=1,lty=2)

points(log10(mean.sr.pred.tank[,1]),log10(kmfit.3y[,1]),col=1,pch=16)
lines(log10(mean.sr.pred.tank[,1]),log10(kmfit.3y[,1]),col=1)


points(log10(mean.sr.pred.tank[,2]),log10(kmfit.5y[,1]),col=2,pch=16)
lines(log10(mean.sr.pred.tank[,2]),log10(kmfit.5y[,1]),col=2)


points(log10(mean.sr.pred.tank[,3]),log10(kmfit.10y[,1]),col=3,pch=16)
lines(log10(mean.sr.pred.tank[,3]),log10(kmfit.10y[,1]),col=3)

legend("bottomright",c('3-years','5-years','10-years'),
       bty='n',col=c(1,2,3),lwd=1,ncol=1,pch=16)
