#ex 10.4
k= 4
n= 10
s= 7.5888
mu= c(8,8,9,12)
phi= sqrt((n*sum((mu-mean(mu))^2))/(k*s))
phi

#ex 10.5
k= 4
n= 10
s= 7.5888
d= 4
phi= sqrt((n*d^2)/(2*k*s))
phi

#ex 10.6
phi=function(k,n,d,s){
p= sqrt((n*d^2)/(2*k*s))
return(p)
}
phi(4,15,3.5,9.383)
phi(4,20,3.5,9.383)
phi(4,18,3.5,9.383)

#ex 10.7
k=4
n=10
s= 9.3833
phi= 2
d= sqrt((2*k*s*phi^2)/n)
d

#ex 10.8
p= function(k){
  n= floor(50/k)
  s= 9.3833
  d= 4.5
 phi= sqrt((n*d^2)/(2*k*s)) 
 return(phi)
}
p(6)
p(5)
p(4)

#ex 10.9
MSG= 3
s= 1.25
v1= 3
v2= 16
fa=qf(0.05,v1,v2, lower.tail = F)
f= (v2*s*fa)/((v2-2)*MSG)
power=pf(f, v1, v2, lower.tail = F)
power

#ex 10.12
north=c(7.7,7.2,7.4,7.6,7.6,7.7,7.7,7.9,8.1,8.4,8.5,8.8)
east=c(6.9,7.0,7.1,7.2,7.3,7.3,7.4,7.6,7.8,8.1,8.3,8.5)
south=c(7.8,7.9,8.1,8.3,8.3,8.4,8.4,8.4,8.6,8.9,9.2,9.4)
west=c(6.4,6.6,6.7,7.1,7.6,7.8,8.2,8.4,8.6,8.7,8.8,8.9)
gmed=median(c(north, east, south, west))
x= data.frame(north, east, south, west)

amed.n= subset(x, select = north, north>median(c(north, east, south, west)))
bmed.n= subset(x, select = north, north<median(c(north, east, south, west)))

amed.e= subset(x, select = east, east>median(c(north, east, south, west)))
bmed.e= subset(x, select = east, east<median(c(north, east, south, west)))

amed.s= subset(x, select = south, south>median(c(north, east, south, west)))
bmed.s= subset(x, select = south, south<median(c(north, east, south, west)))

amed.w= subset(x, select = west, west>median(c(north, east, south, west)))
bmed.w= subset(x, select = west, west<median(c(north, east, south, west)))

c1= length(amed.n$north)+length(bmed.n$north)
c2= length(amed.e$east)+length(bmed.e$east)
c3= length(amed.s$south)+length(bmed.s$south)
c4= length(amed.w$west)+length(bmed.w$west)

r1= length(amed.n$north)+length(amed.e$east)+length(amed.s$south)+length(amed.w$west)
r2= length(bmed.n$north)+length(bmed.e$east)+length(bmed.s$south)+length(bmed.w$west)

e11= r1*(c1/(r1+r2))
e12= r1*(c2/(r2+r2))
e13= r1*(c3/(r1+r2))
e14= r1*(c4/(r1+r2))

e21=r2*(c1/(r1+r2))
e22= r2*(c2/(r2+r2))
e23= r2*(c3/(r1+r2))
e24= r2*(c4/(r1+r2))

chi= (length(amed.n$north)-e11)^2/e11+(length(amed.e$east)-e12)^2/e12+(length(amed.s$south)-e13)^2/e13+(length(amed.w$west)-e14)^2/e14+
  (length(bmed.n$north)-e21)^2/e21+(length(amed.e$east)-e22)^2/e22+(length(amed.s$south)-e23)^2/e23+(length(amed.w$west)-e24)^2/e24

chip= qchisq(0.05,3, lower.tail = F)
if (chi > chip){
   answer= "reject H0"
   paste(answer)
   
}

#method2
north=c(7.7,7.2,7.4,7.6,7.6,7.7,7.7,7.9,8.1,8.4,8.5,8.8)
east=c(6.9,7.0,7.1,7.2,7.3,7.3,7.4,7.6,7.8,8.1,8.3,8.5)
south=c(7.8,7.9,8.1,8.3,8.3,8.4,8.4,8.4,8.6,8.9,9.2,9.4)
west=c(6.4,6.6,6.7,7.1,7.6,7.8,8.2,8.4,8.6,8.7,8.8,8.9)
height=c(north, east, south, west)
n= c(length(north), length(east), length(south), length(west))
side= rep(c("n","e","s","w"), n)
ex10.12= data.frame(side, height)
library(RVAideMemoire)
mood.medtest(height~side, exact=NULL)

#ex10.13
w1= c(60.8,67,65,68.6,61.7)
w2= c(68.7,67.7,75,73.3,71.8)
w3= c(69.6,77.1,75.2,71.5)
w4= c(61.9,64.2,63.1,66.7,60.3)
weight= c(w1, w2, w3, w4)
n= c(length(w1), length(w2), length(w3), length(w4))
feed= rep(1:4, n)
ex10.13= data.frame(weight,feed)
ex10.13= transform(ex10.13, feed= factor(feed))
bartlett.test(weight~feed, data= ex10.13)

#ex11.1
c1=c(28.2,33.2,36.4,34.6,29.1,31.0)
c2=c(39.6,40.8,37.9,37.1,43.6,42.4)
c3=c(46.3,42.1,43.5,48.8,43.7,40.1)
c4=c(41.0,44.1,46.4,40.2,38.6,36.3)
c5=c(56.3,54.1,59.4,62.7,60.0,57.3)
c= c(c1,c2,c3,c4,c5)
n= c(length(c1),length(c2),length(c3),length(c4),length(c5))
water=rep(c('grayson','beaver','angler','appletree','rock'),n)
ex11.1=data.frame(water,c)
aov(c~water)
TukeyHSD(aov(c~water), ordered = T)

#ex11.2
w1= c(60.8,67,65,68.6,61.7)
w2= c(68.7,67.7,75,73.3,71.8)
w3= c(69.6,77.1,75.2,71.5)
w4= c(61.9,64.2,63.1,66.7,60.3)
weight= c(w1, w2, w3, w4)
n= c(length(w1), length(w2), length(w3), length(w4))
feed= rep(1:4, n)
ex11.2= data.frame(weight,feed)
ex11.2= transform(ex11.2, feed= factor(feed))
TukeyHSD(aov(weight~feed, data= ex11.2), ordered= T)

#ex11.3
c1=c(28.2,33.2,36.4,34.6,29.1,31.0)
c2=c(39.6,40.8,37.9,37.1,43.6,42.4)
c3=c(46.3,42.1,43.5,48.8,43.7,40.1)
c4=c(41.0,44.1,46.4,40.2,38.6,36.3)
c5=c(56.3,54.1,59.4,62.7,60.0,57.3)
x243=c(c2,c3,c4)
c= c(c1,c2,c3,c4,c5,x243)
n= c(length(c1),length(c2),length(c3),length(c4),length(c5), length(x243))
water=rep(c('grayson','beaver','angler','appletree','rock','pulled b,an,ap'),n)
ex11.3=data.frame(water,c)
TukeyHSD(aov(c~water), ordered = T)
