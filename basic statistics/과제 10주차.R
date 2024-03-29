#ex20.1
j=seq(1:33)
x1=c(6,1,-2,11,-1,2,5,1,1,3,11,9,5,-3,1,8,-2,3,6,10,4,5,5,3,8,8,6,6,3,5,1,8,10)
x2=c(9.9,9.3,9.4,9.1,6.9,9.3,7.9,7.4,7.3,8.8,9.8,10.5,9.1,10.1,7.2,11.7,8.7,7.6,8.6,10.9,
     7.6,7.3,9.2,7,7.2,7,8.8,10.1,12.1,7.7,7.8,11.5,10.4)
x3=c(5.7,6.4,5.7,6.1,6,5.7,5.9,6.2,5.5,5.2,5.7,6.1,6.4,5.5,5.5,6,5.5,6.2,5.9,5.6,5.8,5.8,5.2
     ,6,5.5,6.4,6.2,5.4,5.4,6.2,6.8,6.2,6.4)
x4=c(1.6,3.0,3.4,3.4,3.0,4.4,2.2,2.2,1.9,0.2,4.2,2.4,3.4,3,0.2,3.9,2.2,4.4,0.2,2.4,2.4,4.4,
     1.6,1.9,1.6,4.1,1.9,2.2,4.1,1.6,2.4,1.9,2.2)
x5=c(2.12,3.39,3.61,1.72,1.8,3.21,2.59,3.25,2.86,2.32,1.57,1.5,2.69,4.06,1.98,2.29,3.55,3.31,1.83
     ,1.69,2.42,2.98,1.84,2.48,2.83,2.41,1.78,2.22,2.72,2.36,2.81,1.64,1.82)
x=data.frame(x1,x2,x3,x4,x5)
cor(x)
model=lm(x5~x1+x2+x3+x4)
summary(model)
anova(model)
rmodel=step(model, direction = "backward")

#ex20.2
x=data.frame(x1,x2,x3,x4,x5)
library(ppcor)
pcor(x)

#ex20.3
rx=data.frame(x5,x1,x4)
newdata=data.frame(x1=7, x4=2)
mdl=lm(x5~x1+x4)
summary(mdl)
predict(mdl, newdata, interval="predict")
