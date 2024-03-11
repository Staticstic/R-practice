install.packages("devtools")
devtools::install_github("RomeroBarata/bimba")
library(bimba)
library(survival)
library(simsurv)
## 공변량 x1~x6
M=1000            #number of iteration
lambdaX = 0.5      #parameter that i set
lambdaC = 9.122
beta = c(1, 1, 1, 1, 1, 1)  #true parameter of covariates
n= 5000



gen.time <- function(data,lambdaX,lambdaC)
{
  
  data$xi= simsurv(dist = "exponential", lambdas =lambdaX ,x= data, 
                   betas =c(x1=beta[1], x2= beta[2], x3= beta[3], x4=beta[4], x5=beta[5], x6=beta[6]))$eventtime
  data$ci= simsurv(dist = "exponential", lambdas =lambdaC ,x= data)$eventtime
  # Calculate ti and di:
  data$ti <- pmin(data$xi, data$ci)
  data$di <- as.numeric(data$xi <= data$ci)
  data$censored = 1-data$di
  return(data)
}


c.base=matrix(NA,M,12)
c.ros=matrix(NA,M,12)
c.smt=matrix(NA,M,12)
c.blsmt=matrix(NA,M,12)
c.adssmt=matrix(NA,M,12)
c.rus=matrix(NA,M,12)
c.enn=matrix(NA,M,12)
c.tomek=matrix(NA,M,12)
c.oss=matrix(NA,M,12)

set.seed(18)
for(j in 1:M){
  #raw
  data.sim <- data.frame(x1=rnorm(n), x2=rnorm(n), x3=runif(n,0,1), x4=runif(n,0,1), x5=rgamma(n, shape=1, scale=1), x6=rgamma(n, shape=1, scale=1))
  
  #base
  data.sample=gen.time(data.sim, lambdaX, lambdaC)
  sample=data.sample[,c(1,2,3,4,5,6,9,10)]
  cox=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample)
  c.base[j,]=c(t(summary(cox)$coefficient[1,c(1,3)]), t(summary(cox)$coefficient[2,c(1,3)]), 
               t(summary(cox)$coefficient[3,c(1,3)]), t(summary(cox)$coefficient[4,c(1,3)]),
               t(summary(cox)$coefficient[5,c(1,3)]), t(summary(cox)$coefficient[6,c(1,3)]))
  ################################oversampling#########################################################
  
  #random oversampling
  sample.ros=ROS(sample, perc_min = 50)
  cox.ros=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample.ros)
  c.ros[j,]=c(t(summary(cox.ros)$coefficient[1,c(1,3)]), t(summary(cox.ros)$coefficient[2,c(1,3)]), 
               t(summary(cox.ros)$coefficient[3,c(1,3)]), t(summary(cox.ros)$coefficient[4,c(1,3)]),
               t(summary(cox.ros)$coefficient[5,c(1,3)]), t(summary(cox.ros)$coefficient[6,c(1,3)]))
  
  #smote
  sample.smt=SMOTE(sample, perc_min = 50)
  cox.smt=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample.smt)
  c.smt[j,]=c(t(summary(cox.smt)$coefficient[1,c(1,3)]), t(summary(cox.smt)$coefficient[2,c(1,3)]), 
              t(summary(cox.smt)$coefficient[3,c(1,3)]), t(summary(cox.smt)$coefficient[4,c(1,3)]),
              t(summary(cox.smt)$coefficient[5,c(1,3)]), t(summary(cox.smt)$coefficient[6,c(1,3)]))
  
  #blsmote
  sample.blsmt=BDLSMOTE(sample, perc_min = 50)
  cox.blsmt=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample.blsmt)
  c.blsmt[j,]=c(t(summary(cox.blsmt)$coefficient[1,c(1,3)]), t(summary(cox.blsmt)$coefficient[2,c(1,3)]), 
              t(summary(cox.blsmt)$coefficient[3,c(1,3)]), t(summary(cox.blsmt)$coefficient[4,c(1,3)]),
              t(summary(cox.blsmt)$coefficient[5,c(1,3)]), t(summary(cox.blsmt)$coefficient[6,c(1,3)]))
  
  #adasyn
  sample.adssmt=ADASYN(sample, perc_min = 50)
  cox.adssmt=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample.adssmt)
  c.adssmt[j,]=c(t(summary(cox.adssmt)$coefficient[1,c(1,3)]), t(summary(cox.adssmt)$coefficient[2,c(1,3)]), 
                t(summary(cox.adssmt)$coefficient[3,c(1,3)]), t(summary(cox.adssmt)$coefficient[4,c(1,3)]),
                t(summary(cox.adssmt)$coefficient[5,c(1,3)]), t(summary(cox.adssmt)$coefficient[6,c(1,3)]))
  
  ################################undersampling#########################################################
  
  #random undersampling
  sample.rus=RUS(sample, perc_maj = 50)
  cox.rus=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample.rus)
  c.rus[j,]=c(t(summary(cox.rus)$coefficient[1,c(1,3)]), t(summary(cox.rus)$coefficient[2,c(1,3)]), 
                 t(summary(cox.rus)$coefficient[3,c(1,3)]), t(summary(cox.rus)$coefficient[4,c(1,3)]),
                 t(summary(cox.rus)$coefficient[5,c(1,3)]), t(summary(cox.rus)$coefficient[6,c(1,3)]))
  
  #edited nearest neighbor
  sample.enn=ENN(sample, remove_class = "Majority")
  cox.enn=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample.enn)
  c.enn[j,]=c(t(summary(cox.enn)$coefficient[1,c(1,3)]), t(summary(cox.enn)$coefficient[2,c(1,3)]), 
              t(summary(cox.enn)$coefficient[3,c(1,3)]), t(summary(cox.enn)$coefficient[4,c(1,3)]),
              t(summary(cox.enn)$coefficient[5,c(1,3)]), t(summary(cox.enn)$coefficient[6,c(1,3)]))
  
  #tomek link
  sample.tomek=TL(sample, remove_class = "Majority")
  cox.tomek=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample.tomek)
  c.tomek[j,]=c(t(summary(cox.tomek)$coefficient[1,c(1,3)]), t(summary(cox.tomek)$coefficient[2,c(1,3)]), 
              t(summary(cox.tomek)$coefficient[3,c(1,3)]), t(summary(cox.tomek)$coefficient[4,c(1,3)]),
              t(summary(cox.tomek)$coefficient[5,c(1,3)]), t(summary(cox.tomek)$coefficient[6,c(1,3)]))
  
  #one sided selection
  sample.oss=OSS(sample)
  cox.oss=coxph(Surv(ti, di)~x1+x2+x3+x4+x5+x6, data=sample.oss)
  c.oss[j,]=c(t(summary(cox.oss)$coefficient[1,c(1,3)]), t(summary(cox.oss)$coefficient[2,c(1,3)]), 
                t(summary(cox.oss)$coefficient[3,c(1,3)]), t(summary(cox.oss)$coefficient[4,c(1,3)]),
                t(summary(cox.oss)$coefficient[5,c(1,3)]), t(summary(cox.oss)$coefficient[6,c(1,3)]))
  
}
cbase=cbind(mean(c.base[,1]), mean(c.base[,2]), mean(c.base[,3]), mean(c.base[,4]), mean(c.base[,5]), mean(c.base[,6]),
            mean(c.base[,7]), mean(c.base[,8]), mean(c.base[,9]), mean(c.base[,10]), mean(c.base[,11]), mean(c.base[,12]))
cros=cbind(mean(c.ros[,1]), mean(c.ros[,2]), mean(c.ros[,3]), mean(c.ros[,4]), mean(c.ros[,5]), mean(c.ros[,6]),
           mean(c.ros[,7]), mean(c.ros[,8]), mean(c.ros[,9]), mean(c.ros[,10]), mean(c.ros[,11]), mean(c.ros[,12]))
csmt=cbind(mean(c.smt[,1]), mean(c.smt[,2]), mean(c.smt[,3]), mean(c.smt[,4]), mean(c.smt[,5]), mean(c.smt[,6]),
           mean(c.smt[,7]), mean(c.smt[,8]), mean(c.smt[,9]), mean(c.smt[,10]), mean(c.smt[,11]), mean(c.smt[,12]))
cbl=cbind(mean(c.blsmt[,1]), mean(c.blsmt[,2]), mean(c.blsmt[,3]), mean(c.blsmt[,4]), mean(c.blsmt[,5]), mean(c.blsmt[,6]),
          mean(c.blsmt[,7]), mean(c.blsmt[,8]), mean(c.blsmt[,9]), mean(c.blsmt[,10]), mean(c.blsmt[,11]), mean(c.blsmt[,12]))
cads=cbind(mean(c.adssmt[,1]), mean(c.adssmt[,2]), mean(c.adssmt[,3]), mean(c.adssmt[,4]), mean(c.adssmt[,5]), mean(c.adssmt[,6]),
           mean(c.adssmt[,7]), mean(c.adssmt[,8]), mean(c.adssmt[,9]), mean(c.adssmt[,10]), mean(c.adssmt[,11]), mean(c.adssmt[,12]))
crus=cbind(mean(c.rus[,1]), mean(c.rus[,2]), mean(c.rus[,3]), mean(c.rus[,4]), mean(c.rus[,5]), mean(c.rus[,6]),
           mean(c.rus[,7]), mean(c.rus[,8]), mean(c.rus[,9]), mean(c.rus[,10]), mean(c.rus[,11]), mean(c.rus[,12]))
cenn=cbind(mean(c.enn[,1]), mean(c.enn[,2]), mean(c.enn[,3]), mean(c.enn[,4]), mean(c.enn[,5]), mean(c.enn[,6]),
           mean(c.enn[,7]), mean(c.enn[,8]), mean(c.enn[,9]), mean(c.enn[,10]), mean(c.enn[,11]), mean(c.enn[,12]))
ctomek=cbind(mean(c.tomek[,1]), mean(c.tomek[,2]), mean(c.tomek[,3]), mean(c.tomek[,4]), mean(c.tomek[,5]), mean(c.tomek[,6]),
             mean(c.tomek[,7]), mean(c.tomek[,8]), mean(c.tomek[,9]), mean(c.tomek[,10]), mean(c.tomek[,11]), mean(c.tomek[,12]))
coss=cbind(mean(c.oss[,1]), mean(c.oss[,2]), mean(c.oss[,3]), mean(c.oss[,4]), mean(c.oss[,5]), mean(c.oss[,6]),
           mean(c.oss[,7]), mean(c.oss[,8]), mean(c.oss[,9]), mean(c.oss[,10]), mean(c.oss[,11]), mean(c.oss[,12]))
result=rbind(cbase,cros,csmt,cbl,cads,crus,cenn,ctomek,coss)
result
write.csv(result,file="C:/Users/j/Desktop/논문/bias6_30_5000.csv")
