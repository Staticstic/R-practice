#ex8.4
#method1
size = power.t.test(n=, delta = 0.5, sd = sqrt(0.52), sig.level = 0.05, power = 0.9, type = "two.sample", alternative = "two.sided")
size
ceiling(size$n)
nn= round(size$n, 1)
n2 = function(n1, n){
  n2= ceiling((n*n1)/(2*n1-n))
  return(n2)
}                                           
n2(30, nn)

#method2
library(pwr)
del=0.5/sqrt(0.52)
pwr.t.test(n=, d= del, sig.level = 0.05, power= 0.9, type= "two.sample", alternative = "two.sided")
ceiling(pwr.t.test(n=, d= del, sig.level = 0.05, power= 0.9, type= "two.sample", alternative = "two.sided")$n)
n2 = pwr.t2n.test(n1= 30, n2=, d= del, sig.level = 0.05, power= 0.9, alternative = "two.sided")
ceiling(n2$n2)

#ex8.5
dd=power.t.test(n=20, delta =, sd = sqrt(0.52), sig.level = 0.05, power = 0.9, type = "two.sample", alternative = "two.sided")
round(dd$delta, 2)

#ex8.6
power.t.test(n=15, delta = 1, sd = sqrt(0.52), sig.level = 0.05, power = , type = "two.sample", alternative = "two.sided")

# 정규근사를 이용한 power구하기
na = function(n, d,alpha, sp){
  v = 2*(n-1)
  tb = round(d/sqrt(2*sp/n)- qt(alpha/2,v, lower.tail = F),3)
  z = pnorm(tb)
  return(z)
}
na(15,1,0.05,0.5193)

# phi
ps = function(n, d, sp){
  v = 2*(n -1)
  phi= round(sqrt((n*d^2)/(4*sp)),2)
  return(phi)
}
ps(15,1,0.5193)

# ex8.7
trap1 = c(41, 35, 33, 36, 40, 46, 31, 37, 34, 30, 38)
trap2 = c(52, 57, 62, 55, 64, 57, 56, 55, 60, 59)

# method1
vartest= function(x, y, alpha){
  v1= length(x)-1
  v2= length(y)-1
  s1= var(x)
  s2= var(y)
  if (s1 > s2){
    f=  round(s1/s2,2)
  }
  else if (s1 < s2){
    f= round(s2/s1,2)
  }
  fp = qf(alpha/2, v1, v2, lower.tail = F)
  if (f > fp){
    answer= (c("reject H0", "sp=" (s1*v1+s2*v2)/(v1+v2)))
  }
  else if (f <= fp){
    answer= ("cannot reject H0")
  }
  pv= round(pf(f,v1,v2, lower.tail = F)*2,2)
  return(c(answer, pv))
}
vartest(trap1, trap2, 0.05)

# method2
var.test(trap1, trap2)

# ex8.8
house= c(69.3,75.5,81,74.7,72.3,78.7,76.4)
out= c(69.5,64.6,74,84.8,76,93.9,81.2,73.4,88)
# method1
vartest= function(x, y, alpha){
  v1= length(x)-1
  v2= length(y)-1
  s1= var(x)
  s2= var(y)
  f=  round(s2/s1,2)
  fp= qf(0.05,8,6, lower.tail = F)
  if (f > fp){
    answer= ("reject H0")
  }
  else if (f <= fp){
    answer= ("cannot reject H0")
  }
  pv= round(pf(f, 8,6, lower.tail = F), 3) 
  return(c(answer, pv))
}
vartest(house, out, 0.05)

# method2
var.test(house, out, alternative = "less")

# ex8.9
trap1 = c(41, 35, 33, 36, 40, 46, 31, 37, 34, 30, 38)
trap2 = c(52, 57, 62, 55, 64, 57, 56, 55, 60, 59)
# method1
levenetest= function(x, y, alpha){
  n1= length(x)
  n2= length(y)
  v1= n1-1
  v2= n2-1
  xp= round(abs(x-mean(x)),2)
  xb= round(mean(xp),2)
  yp= round(abs(y-mean(y)),2)
  yb= round(mean(yp),2)
  ss1= round(sum(xp^2)-(sum(xp))^2/n1,2)
  ss2= round(sum(yp^2)-(sum(yp))^2/n2,2)
  sp= (ss1+ss2)/(v1+v2)
  ssp= round(sqrt(sp/n1+sp/n2),2)
  t= (xb-yb)/ssp
  tp= round(qt(alpha/2, v1+v2, lower.tail = F),3)
  if (abs(t)>tp){
    answer= "reject H0"
  }
  else if (abs(t)<tp){
    answer= "cannot reject H0"
  }
  pv= round(pt(t,v1+v2,lower.tail = F)*2,2)
  return(c(answer, pv))
  
}
levenetest(trap1, trap2, 0.05)

# method2
library(lawstat)
time= c(41, 35, 33, 36, 40, 46, 31, 37, 34, 30, 38, 52, 57, 62, 55, 64, 57, 56, 55, 60, 59)
trap= c( 1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2)
levene.test(time,trap, location="mean")

# ex8.10
weight=c(72.5, 71.7, 60.8, 63.2, 71.4, 73.1, 77.9, 75.7, 72, 69)
height=c(183, 172.3, 180.1, 190.2, 191.4, 169.6, 166.4, 177.6, 184.7, 187.5, 179.8)

# 8.10a
vratiotest= function(x, y, alpha){
  n1= length(x)
  n2= length(y)
  v1= n1-1
  v2= n2-1
  logx= log10(x)
  logy= log10(y)
  ss1= round(sum(logx^2)-(sum(logx))^2/n1,2)
  ss2= round(sum(logy^2)-(sum(logy))^2/n2,2)
  vlogx= var(logx)
  vlogy= var(logy)
  f= round(vlogx/vlogy,2)
  fp = qf(alpha/2, 9, 10, lower.tail = F)
  if (f> fp){
    answer= "reject H0"
  }
  else if (f <= fp){
    answer= "cannot reject H0"
  }
  pv= round(pf(2.74, 9, 10, lower.tail = F)*2,2)
  return(c(answer,pv))
}
vratiotest(weight, height, 0.05)

# 8.10b
coeftest= function(x, y, alpha){
  n1= length(x)
  n2= length(y)
  v1= n1-1
  v2= n2-1
  cf1= sd(x)/mean(x)
  cf2= sd(y)/mean(y)
  vp= round((v1*cf1+v2*cf2)/(v1+v2),4)
  vps= round(vp^2,6)
  z= round((cf1-cf2)/sqrt((vps/v1+vps/v2)*(0.5+vps)),2)
  zp= qnorm(alpha/2, lower.tail = F)
  if (abs(z)> zp){
    answer= "reject H0"
  }
  else if (abs(z) <= zp){
    answer= "cannot reject H0"
  }
  pv= round(pnorm(z, lower.tail = F)*2,2)
  return(c(answer, pv))
}
coeftest(weight, height, 0.05)

# method2
# install.packages("Zar5", repos="http://R-Forge.R-project.org")
library("Zar5")
CV.test(weight, height, test = "F")
CV.test(weight, height, test = "Z")   

# ex8.11
male= c(193,188,185,183,180,175,170)
female= c(178,173,168,165,163)
mwtest= function(x, y, alpha){
  n1= length(x)
  n2= length(y)
  nr= n1+1
  nn= n1+n2
  rk=rank(-c(x, y), ties.method="average")
  rk
  xrank=rk[1:length(x)]
  yrank=rk[nr:nn]
  r1= sum(xrank)
  r2= sum(yrank)
  u= n1*n2+ n1*(n1+1)/2 - r1
  up= n1*n2 - u
  pu= qwilcox(alpha/2, n1, n2, lower.tail = F)+1
  if(max(u, up)>pu){
    answer= "reject H0"
  }
  else if(max(u, up)<=pu){
    answer= "cannot reject H0"
  }
  pv= round(pwilcox(u-1, n1,n2, lower.tail = F)+pwilcox(up,n1,n2, lower.tail = T),3)
  return(c(answer,pv))
}
mwtest(male, female, 0.05)

# method2
wilcox.test(male, female)

# ex8.12
training= c(44,48,36,32,51,45,54,56)
notrain= c(32,40,44,44,34,30,26)
mwostest= function(x, y, alpha){
  n1= length(x)
  n2= length(y)
  nr= n1+1
  nn= n1+n2
  rk=rank(c(x, y), ties.method="average")
  rk
  xrank=rk[1:length(x)]
  yrank=rk[nr:nn]
  r1= sum(xrank)
  r2= sum(yrank)
  u= n1*n2+ n1*(n1+1)/2 - r1
  up= n1*n2 - u
  pu= qwilcox(alpha, n1, n2, lower.tail = F)+1
  if(max(u,up)>pu){
    answer= "reject H0"
  }
  else if(max(u,up)<=pu){
    answer= "cannot reject H0"
  }
  pv= round(pwilcox(max(u,up), n1,n2, lower.tail = F),3)
  return(c(answer,pv))
}
mwostest(training, notrain, 0.05)

# method2
wilcox.test(training, notrain, alternative = "greater")

# ex8.13
ex13= function(n1, n2, u, alpha){
  n= n1+n2
  up = n1*n2-u
  mu= n1*n2/2
  su= sqrt((n1*n2*(n+1))/12)
  z= (up-mu)/su
  zp= qnorm(alpha, lower.tail = F)
  if(z>zp){
    answer= "reject H0"
  }
  else if(z<=zp){
    answer= "cannot reject H0"
  }
  pv= round(pnorm(z, lower.tail = F),4)
  return(c(answer, pv))
}
ex13(22,46,282,0.05)

# ex8.14
#method1
a= c("a","a","a","b","d","d","f","f","g","g","h")
b= c("a","a","c","c","d","e","g","g","h","j","j","j","j","k")
a= factor(a, levels = letters)
a= as.numeric(a)
b= factor(b, levels = letters)
b= as.numeric(b)
a
b

mwtest= function(x, y, alpha){
  n1= length(x)
  n2= length(y)
  nr= n1+1
  nn= n1+n2
  rk=rank(c(x, y), ties.method="average")
  xrank=rk[1:length(x)]
  yrank=rk[nr:nn]
  r1= sum(xrank)
  r2= sum(yrank)
  u= n1*n2+ n1*(n1+1)/2 - r1
  up= n1*n2 - u
  pu= qwilcox(alpha/2, n1, n2, lower.tail = F)+1
  if(max(u, up)>pu){
    answer= "reject H0"
  }
  else if(max(u, up)<=pu){
    answer= "cannot reject H0"
  }
  pv= round(pwilcox(u-1, n1,n2, lower.tail = F)+pwilcox(up,n1,n2, lower.tail = T),3)
  return(c(answer,pv))
}
mwtest(a, b, 0.05)

# method2
wilcox.test(a,b)

#ex8.15
# method1
mwtest= function(x, y, alpha){
  n1= length(x)
  n2= length(y)
  nr= n1+1
  nn= n1+n2
  rk=rank(c(a, b), ties.method="average")
  xrank=rk[1:length(x)]
  yrank=rk[nr:nn]

  xa= xrank[xrank < median(rk)]
  f11= length(xa)
  xb= xrank[xrank > median(rk)]
  f12= length(xb)
  ya= yrank[yrank < median(rk)]
  f21= length(ya)
  yb= yrank[yrank > median(rk)]
  f22= length(yb)
  
  c1= f11+f12
  c2= f21+f22
  r1= f11+f21
  r2= f12+f22
  n= f11+f12+f21+f22
  chi= (n*(abs(f11*f22-f12*f21)-n/2)^2)/(c1*c2*r1*r2)
  pchi= qchisq(alpha, 1, lower.tail = F)
  if(chi>pchi){
    answer= "reject H0"
  }
  else if(chi<=pchi){
    answer= "cannot reject H0"
  }
  pv= round(pchisq(chi, 1, lower.tail = F),2)
  return(c(answer, pv))
}
mwtest(a,b,0.05)

# method2
mood.test(a,b, alternative = "two.sided")

# ex8.16
library(vegan)
m= c(47,35,7,5,3,2)
l= c(48,23,11,13,8,2)
indicetest= function(x,y,alpha){
  h1= diversity(x, base= 10)
  sh1= (sum(x*(log10(x))^2)-((sum(x*log10(x))^2)/sum(x)))/sum(x)^2
  h2= diversity(y, base = 10)
  sh2= (sum(y*(log10(y))^2)-((sum(y*log10(y))^2)/sum(y)))/sum(y)^2
  sh= sqrt(sh1+sh2)
  t= (h1-h2)/sh
  v= ceiling((sh1+sh2)^2/((sh1)^2/sum(x)+(sh2^2)/sum(y)))
  tp= qt(alpha/2, v, lower.tail = F)
  if(abs(t)>tp){
    answer= "reject H0"
  }
  else if(abs(t)<=tp){
    answer= "cannot reject H0"
  }
  pv= round(pt(abs(t),v, lower.tail = F)*2,3)
  return(c(answer, pv))
}
indicetest(m,l,0.05)

# ex9.1
hind=c(142,140,144,144,142,146,149,150,142,148)
fore=c(138,136,147,139,143,141,143,145,136,146)
t.test(hind, fore, alternative = "two.sided", paired = T )

# ex9.2
new = c(2250,2410,2260,2200,2360,2320,2240,2300,2090)
old = c(1920,2020,2060,1960,1960,2140,1980,1940,1790)
t.test(new, old, mu=250, alternative = "greater", paired = T )

# ex9.3
library(PairedData)
hind=c(142,140,144,144,142,146,149,150,142,148)
fore=c(138,136,147,139,143,141,143,145,136,146)
pitman.morgan.test.default(hind, fore, alternative = "two.sided")

# ex9.4
hind=c(142,140,144,144,142,146,149,150,142,148)
fore=c(138,136,147,139,143,141,143,145,136,146)
diff= paired(hind, fore)
wilcox.test.paired(diff)
