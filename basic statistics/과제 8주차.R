#ex14.1
sp=rep(c('sp1','sp2','sp3'),each=24)
temp=rep(rep(c('low','mid','high'),each=8),3)
sex=rep(rep(rep(c('male','female'),each=4),3),3)
rate=c(1.9,1.8,1.6,1.4,1.8,1.7,1.4,1.5,2.3,2.1,2.0,2.6,2.4,2.7,2.4,2.6,2.9,2.8,3.4,3.2,3.0,3.1,3.0,2.7,
       2.1,2.0,1.8,2.2,2.3,2.0,1.9,1.7,2.4,2.6,2.7,2.3,2.0,2.3,2.1,2.4,3.6,3.1,3.4,3.2,3.1,3.0,2.8,3.2,
       1.1,1.2,1.0,1.4,1.4,1.0,1.3,1.2,2.0,2.1,1.9,2.2,2.4,2.6,2.3,2.2,2.9,2.8,3.0,3.1,3.2,2.9,2.8,2.9)
data.frame(sp,temp,sex,rate)
summary(aov(rate~sp+temp+sex+sp:temp+sp:sex+temp:sex+sp:temp:sex))

#ex15.1
drug=rep(c('1','2','3'),each=4)
sc=rep(c('A','Q','D','B','L','S'),each=2)
chol=c(102,104,103,104,108,110,109,108,104,106,105,107)
data.frame(drug, sc, chol)
ex15.1=lm(chol~drug/sc)
anova(ex15.1)

#ex15.2
hormone=as.factor(rep(c(rep('No',10),rep('Yes',10)),3))
gender=as.factor(rep(rep(c(rep('M',5),rep('F',5)),2),3))
calcium=c(15.3,17.4,10.9,10.3,6.7,16.3,20.4,12.4,
          15.8,9.5,34,22.8,27.8,25,29.3,38.1,26.2,32.3,35.8,30.2)
for(i in 1:length(calcium)){
  print(rnorm(2,calcium[i],1))
}
animal=as.factor(rep(c('1','2','3'), each=20))
cal=c(15.3,17.4,10.9,10.3,6.7,16.3,20.4,12.4,15.8,9.5,34,22.8,27.8,25,29.3,38.1,26.2,32.3,35.8,30.2,
      13.3,17.3,10.8,10.2,6.8,17.0,20.9,13.3,16.0,10.1,32.6,23.8,26.1,24.3,28.4,38.1,26.0,33.7,35.0,30.4,
      15.8,18.8,10.9,10.0,6.7,18.0,21.3,12.6,16.2,7.3,34.0,21.9,28.7,25.9,28.6,39.0,24.6,31.9,37.0,30.4)
ex15.2=data.frame(animal, hormone, gender, cal)
model=lm(cal~animal/(hormone+gender)+animal/(hormone:gender))
anova(model)
