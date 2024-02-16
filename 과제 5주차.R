#ex 10.1
w1= c(60.8,67,65,68.6,61.7)
w2= c(68.7,67.7,75,73.3,71.8)
w3= c(69.6,77.1,75.2,71.5)
w4= c(61.9,64.2,63.1,66.7,60.3)
weight= c(w1, w2, w3, w4)
n= c(length(w1), length(w2), length(w3), length(w4))
# ex10.1a
sum1= sum(w1)
sum2= sum(w2)
sum3= sum(w3)
sum4= sum(w4)
sumx= c(sum(w1), sum(w2), sum(w3), sum(w4))
tot= sum(weight)

mean1= mean(w1)
mean2= mean(w2)
mean3= mean(w3)
mean4= mean(w4)
meanx= c(mean(w1), mean(w2), mean(w3), mean(w4))
omean= mean(weight)

sst= sum((weight-omean)^2)
df= length(weight)-1
ssg= sum(n*(meanx-omean)^2)
gdf= length(n)-1
sse= sum((w1-mean1)^2)+sum((w2-mean2)^2)+sum((w3-mean3)^2)+sum((w4-mean4)^2)
edf= sum(n-1)

# ex10.1b
ss= sum(sumx^2/n)
c= sum(tot)^2/length(weight)
tss= sum(weight^2)-c
gss= ss-c 
ess= tss-gss

# ex10.1c
msg= ssg/gdf
mse= sse/edf
f= msg/mse
fp= qf(0.05, 3, 15, lower.tail = F)
if(f>fp){
  answer= "reject H0"
}else{
  answer= "cannot reject H0"
}
pv= round(pf(f,3,15, lower.tail = F),5)
print(c(answer, paste("p-value=", pv)))

# method2
w1= c(60.8,67,65,68.6,61.7)
w2= c(68.7,67.7,75,73.3,71.8)
w3= c(69.6,77.1,75.2,71.5)
w4= c(61.9,64.2,63.1,66.7,60.3)
weight= c(w1, w2, w3, w4)
n= c(length(w1), length(w2), length(w3), length(w4))
feed= rep(1:4, n)
ex10.1= data.frame(weight,feed)
ex10.1= transform(ex10.1, feed= factor(feed))

res=aov(weight~feed, data= ex10.1)
residual= residuals(object=res)
shapiro.test(residual)
bartlett.test(weight~feed, data= ex10.1)

summary(aov(weight~feed, data= ex10.1))

boxplot(weight~feed, xlab= "type of feed", ylab="weight of pig")

# ex10.2
p1= c(34,36,34,35,34)
p2= c(37,36,35,37,37)
p3= c(34,37,35,37,36)
p4= c(36,34,37,34,35)
phos= c(p1, p2, p3, p4)
n= c(length(p1), length(p2), length(p3), length(p4))
tech= rep(1:4, n)
ex10.2= data.frame(phos,tech)
ex10.2= transform(ex10.2, tech= factor(tech))

res=aov(phos~tech, data= ex10.2)
residual= residuals(object=res)
shapiro.test(residual)
bartlett.test(phos~tech, data= ex10.2)
summary(aov(phos~tech,data=ex10.2))
model = lm(phos ~tech,data=ex10.2)
anova(model)

boxplot(phos~tech, xlab = "type of technician", ylab = "phosphorus content")

# ex10.3
library(onewaytests)
po1=c(27.9,27,26,26.5,27,27.5)
po2=c(24.2,24.7,25.6,26,27.4,26.1)
po3=c(29.1,27.7,29.9,30.7,28.8,31.1)
pot=c(po1, po2, po3)
wheat=rep(c("G","A","L"), c(6,6,6))
ex10.3= transform(data.frame(pot, wheat), wheat=factor(wheat))

model= lm(pot~wheat)
res= model$residuals

shapiro.test(res)
bartlett.test(pot~wheat,data= ex10.3)
welch.test(pot~wheat, data= ex10.3)
oneway.test(pot~wheat, data= ex10.3, var.equal = F)
oneway.test(pot~wheat, data= ex10.3, var.equal = T)

boxplot(pot~wheat, xlab = "variety of wheat", ylab = "potassium content")

