#ex8.1 
B= c(8.8, 8.4, 7.9, 8.7, 9.1, 9.6)
G= c(9.9, 9.0, 11.1, 9.6, 8.7, 10.4, 9.5)

twosidettest = function(x, y, alpha ) {
  n1= length(x)
  n2= length(y)
  v1= n1-1
  v2= n2-1
  v= v1+v2
  xbar1= mean(x)
  xbar2= mean(y)
  ss1= sum(x^2)-(sum(x))^2/n1
  ss2= sum(y^2)-(sum(y))^2/n2
  sp=(ss1+ss2)/(v1+v2)
  ssx=sqrt((sp/n1)+(sp/n2))
  t= (xbar1-xbar2)/ssx
  tp= round(qt(alpha/2, v, lower.tail = F), 3)
  if (abs(t)>= tp){
        answer = c('reject H0')
  }else if (abs(t)<tp){
        answer = c('not reject H0')
  }
  p = round(2 * pt(abs(t), v, lower.tail = F),3)
  return(c(answer, p))
}
twosidettest(B, G, 0.05)

t.test(B, G, var.equal = T )

#ex8.2
P= c(48.2, 54.6, 58.3, 47.8, 51.4, 52, 55.2, 49.1, 49.9, 52.6)
N= c(52.3, 57.4, 55.6, 53.2, 61.3, 58.0, 59.8, 54.8)

lowsidettest = function(x, y, alpha ) {
  n1= length(x)
  n2= length(y)
  v1= n1-1
  v2= n2-1
  v= v1+v2
  xbar1= mean(x)
  xbar2= mean(y)
  ss1= sum(x^2)-(sum(x))^2/n1
  ss2= sum(y^2)-(sum(y))^2/n2
  sp=(ss1+ss2)/(v1+v2)
  ssx=sqrt((sp/n1)+(sp/n2))
  t= (xbar1-xbar2)/ssx
  tp= round(qt(alpha, v, lower.tail = F), 3)
  if (t <= tp){
    answer = c('reject H0')
  }else if (t > tp){
    answer = c('not reject H0')
  }
  p = round(pt(t, v),4)
  return(c(answer, p))
}
lowsidettest(P, N, 0.05)

t.test(P, N, var.equal = T, alternative = "less")

#ex8.2a
tmp30= c(40, 38, 32, 37, 39, 41, 35)
tmp10= c(36, 45, 32, 52, 59, 41, 48, 55)

BFttest = function(x, y, alpha ) {
  n1= length(x)
  n2= length(y)
  v1= n1-1
  v2= n2-1
  xbar1= mean(x)
  xbar2= mean(y)
  vx1= var(x)
  vx2= var(y)
  sx1= vx1/n1 
  sx2= vx2/n2
  ssx= sqrt(sx1+sx2)
  t= (xbar1-xbar2)/ssx
  v= (vx1+vx2)^2/((vx1^2)/v1 + (vx2^2)/v2) 
  tp= round(qt(alpha/2, v, lower.tail = F), 3)
  if (abs(t)>= tp){
    answer = c('reject H0')
  }else if (abs(t)<tp){
    answer = c('not reject H0')
  }
  p = round(2 * pt(abs(t), v, lower.tail = F),3)
  return(c(answer, p))
}
BFttest(tmp30, tmp10, 0.05)

library("asht")
bfTest(tmp30, tmp10, alternative = "two.sided", mu = 0, conf.level = 0.95)

#ex8.3
samplesize= function(x, y, alpha, d, z){
  n= z
  while(n) {
  n1= length(x)
  n2= length(y)
  v1= n1-1
  v2= n2-1
  v= v1+v2
  ss1= sum(x^2)-(sum(x))^2/n1
  ss2= sum(y^2)-(sum(y))^2/n2
  sp= round((ss1+ss2)/(v1+v2),3)
  
  tp= round(qt(alpha/2, 2*(n-1), lower.tail = F),3)
  sn= (2*sp*tp^2)/d^2
  nsn= ceiling(sn)
  if (nsn == n) {
    break
  }
  n = nsn
  }
  return(nsn)
}
samplesize(B,G,0.05,0.5,50)

n1= 14
n = samplesize(B,G,0.05,0.5,50)
n2 = ceiling((n*n1)/(2*n1-n))
n2
