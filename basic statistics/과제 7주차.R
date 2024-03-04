#ex12.1-2
hormone=as.factor(c(rep('No',10),rep('Yes',10)))
gender=as.factor(rep(c(rep('M',5),rep('F',5)),2))
calcium=c(15.3,17.4,10.9,10.3,6.7,16.3,20.4,12.4,
          15.8,9.5,34,22.8,27.8,25,29.3,38.1,26.2,32.3,35.8,30.2)
ex12.1=data.frame(hormone,gender,calcium)
x=aov(calcium~hormone+gender+hormone:gender)
summary(aov(calcium~hormone+gender+hormone:gender))

#ex12.3
sd= anova(x)["Residuals", "Mean Sq"]
nohor=subset(ex12.1, hormone=="No")
yeshor=subset(ex12.1, hormone=="Yes")
nomean = mean(nohor$calcium)
noleng = length(nohor$calcium)
yesmean = mean(yeshor$calcium)
yesleng = length(yeshor$calcium)

t=qt(0.025,16, lower.tail = F)
noLL= nomean-t*sqrt(sd/noleng)
noUL= nomean+t*sqrt(sd/noleng)
yesLL= yesmean-t*sqrt(sd/yesleng)
yesUL= yesmean+t*sqrt(sd/yesleng)

#ex12.4
trt=as.factor(rep(c('1','2','3'),5))
block=as.factor(rep(c('1','2','3','4','5'),each=3))
time=c(8.25,11.25,10.75,11,12.5,11.75,10.25,12,11.25,9.5,9.75,9,8.75,11,10)
ex12.4=data.frame(block,trt,time)
library(lme4)
lmer(time ~ trt + block+(1|block))
anova(lmer(time ~ trt + block+(1|block)))
pf(anova(lmer(time ~ trt + block+(1|block)))["trt","F value"],2,8, lower.tail = F)

#12.6
person=rep(seq(1,8),5)
attack=c(0,1,0,1,0,0,0,0,0,1,0,1,1,1,0,0,0,1,0,0,1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,1,0)
type=rep(c('ll','lt','dl','ds','no'),each=8)
ex12.6=data.frame(person,type, attack)
library(RVAideMemoire)
cochran.qtest(attack~type|person)

#ex13.1
a= rep(c('1','2','3'),each=2)
b= rep(c('1','2'),3)
x=c(10,20,30,60,60,120)
data.frame(a,b,x)
summary(aov(x~a+b))
logx=log10(x)
data.frame(a,b,logx)
summary(aov(logx~a+b))

#ex13.2
x1=c(3.1,2.9,3.3,3.6,3.5)
x2=c(7.6,6.4,7.5,6.9,6.3)
logx1=log10(x1+1)
logx2=log10(x2+1)
t.test(logx1)
t.test(x1)

#ex13.3
g1=c(2,0,2,3,0)
g2=c(6,4,8,2,4)
g3=c(9,5,6,5,11)
g4=c(2,4,1,0,2)
g1t=sqrt(g1+0.5)
g2t=sqrt(g2+0.5)
g3t=sqrt(g3+0.5)
g4t=sqrt(g4+0.5)
data.frame(g1t,g2t,g3t,g4t)

#ex13.4
in1=c(84.2,88.9,89.2,83.4,80.1,81.3,85.8)
in2=c(92.3,95.1,90.3,88.6,92.6,96,93.7)

in1t=asin(sqrt(in1/100))*(180/pi)
in2t=asin(sqrt(in2/100))*(180/pi)
t.test(in1t)
back=(sin(in1t*(pi/180))^2)*100
t.test(back)
       