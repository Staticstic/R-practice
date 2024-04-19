#Ex1.1a
counts=c(56,60,46,49)
barplot(counts, ylab="Number of Nests", xlab="Nest Site", names.arg = c("A","B","C","D"), cex.names = 0.9, col="gray")

#Ex1.1b
barplot(counts, ylab="Number of Nests", xlab="Nest Site", names.arg = c("A","B","C","D"), cex.names = 0.9, col="cyan",
        ylim=c(45,60),xpd=F, yaxp=c(45,60,3))
#Ex1.2
counts=c(13,68,44,21,8)
barplot(counts, ylab="Number of Fish", xlab="Pigmentation Class", names.arg = c("0","1","2","3","4"), cex.names = 1.0)

#Ex1.3
counts=c(10,27,22,4,1)
barplot(counts, ylab="Number of Litters", xlab="Litter Size", names.arg = c("3","4","5","6","7"))

#Ex1.4a
aphids = seq(0, 41)
plants = c(3,1,1,1,2,3,5,7,8,11,10,11,13,12,16,13,14,16,15,14,17,18,23,17,19,18,19,21,18,13,10,14,9,10,8,5,4,1,2,1,0,1)
barplot(plants, names.arg = aphids, cex.names = 0.8, ylim = c(0,25), ylab = "Frequency of Observations", xlab = "Observerd Number of Aphids per plants")

#Ex1.4b
plants=c(6,17,40,54,59,75,77,55,32,8,1)
barplot(plants, names.arg=c("0-3","4-7","8-11","12-15","16-19","20-23","24-27","28-31","32-35","36-39","40-43"), 
        ylab="Frequency of Observations", xlab="Observed Number of Aphids per Plants", ylim=c(0,80), yaxp=c(0,80,8))

#Ex1.5a
df= as.data.frame(cbind(x= seq(8.15,9.25,length=11), Freq= c(2,6,8,11,17,17,24,18,13,10,4)))
df                  
df.freq= as.vector(rep(df$x, df$Freq))                  
hist(df.freq, breaks=seq(8.15,9.25,length=12),main="Determinations of the Amount of Phosphorus in Leaves", ylab="Frequency", xlab="Phosphorus", ylim=c(0,30),yaxp=c(0,30,6))
lines(df$x,df$Freq)

#Ex1.5b
x=seq(8.2, 9.2, 0.1)
frq=c(2,6,8,11,17,17,24,18,13,10,4)
y=data.frame(x,frq)
plot(y, type='o', ylab= "Frequency", xlab= "Phosphorus(mg/g of leaf)", ylim=c(0,30), xaxp=c(8.2,9.2,10))
rfrq=frq/sum(frq)
par(new=T)
plot(rfrq, type="n",ylim=c(0,0.23), ylab="", xlab="", axes=F)
axis(side=4, at=NULL, labels = T, las=1)

#EX1.5c
cumfrq=cumsum(frq)
z=data.frame(x,cumfrq)
rcumfrq= cumfrq/sum(frq)
plot(z, type="o", ylab="Cumluative Frequency", xlab="Phosphorus(mg/g of leaf",ylim=c(0,140),xaxp=c(8.2,9.2,10))
par(new=T)
zr=data.frame(x,rcumfrq)
plot(zr, type="n",ylim=c(0,1.08), ylab="", xlab="",xaxp=c(8.2,9.2,10),axes = F)
axis(side=4, at=NULL, labels = T)

#Ex1.5d
w=rep(130,11)
v=w-cumfrq
rv=v/sum(frq)
inv=data.frame(x,v)
plot(inv,type="o", ylab="Cumluative Frequency", xlab="Phosphorus(mg/g of leaf",ylim=c(0,140),xaxp=c(8.2,9.2,10))
par(new=T)
rinv=data.frame(x,rv)
plot(rinv, type="n",ylim=c(0,1.08), ylab="", xlab="",xaxp=c(8.2,9.2,10),axes = F)
axis(side=4, at=NULL, labels = T)

#Ex3.1
X=c(3.3,3.5,3.6,3.6,3.7,3.8,3.8,3.8,3.9,3.9,3.9,4.0,4.0,4.0,4.0,4.1,4.1,4.1,4.2,4.2,4.3,4.3,4.4,4.5)
Xbar=mean(X)
round(Xbar,2)

#Ex3.2
install.packages("Hmisc")
library(Hmisc)
xi=seq(3.3,4.5,0.1)
fi=c(1,0,1,2,1,3,3,4,3,2,2,1,1)
wtd.mean(xi, weights=fi)
x.median=wtd.quantile(xi, weights=fi, probs = c(.5), type=c("(i-1)/(n-1)"))
x.median

#Ex3.3
A=c(16,32,37,39,40,41,42,50,82)
B=c(34,36,38,45,50,54,56,59,69,91)
xa=c(mean(A),median(A))
xb=c(mean(B),median(B))
xa
xb

#Ex3.4
install.packages("psych")
library(psych)
RX=c(1.05,1.10,1.20,1.31)
XRX=c(mean(RX),geometric.mean(RX))
XRX

#Ex3.5
v=c(40,20)
xv=c(mean(v),harmonic.mean(v))
xv

#Ex3.6
x1=c(842,844,846,846,847,848,849)
cx1=x1-840
mean(x1)
mean(cx1)
mean(cx1)-(-840)
x2=c(8000,9000,9500,11000,12500,13000)
cx2=x2/1000
mean(x2)
mean(cx2)
mean(cx2)/0.001

#Ex4.1
xi=c(1.2,1.4,1.6,1.8,2.0,2.2,2.4)
round(sum(xi-mean(xi)),2)
md=sum(abs(xi-mean(xi)))
sum((xi-mean(xi))^2)
mean(xi)
rangex=max(xi)-min(xi)
library(stats)
IQR(xi, type=1)
md/length(xi)
var(xi)
sd(xi)
xi2=c(1.2,1.6,1.7,1.8,1.9,2.0,2.4)
round(sum(xi2-mean(xi2)),2)
md2=sum(abs(xi2-mean(xi2)))
sum((xi2-mean(xi2))^2)
mean(xi2)
rangex2=max(xi2)-min(xi2)
IQR(xi2, type=1)
md2/length(xi2)
var(xi2)
sd(xi2)

#Ex4.2
x1=c(1.2,1.4,1.6,1.8,2.0,2.2,2.4)
mean(x1)
ss1=sum(x1^2)-sum(x1)^2/length(x1)
sv1=ss1/(length(x1)-1)
sqrt(sv1)
cv1=sqrt(sv1)/mean(x1)
cv1
x2=c(1.2,1.6,1.7,1.8,1.9,2.0,2.4)
mean(x2)
ss2=sum(x2^2)-sum(x2)^2/length(x2)
sv2=ss2/(length(x2)-1)
sqrt(sv2)
cv2=sqrt(sv2)/mean(x2)
cv2

#Ex4.3
install.packages("vegan")
library(vegan)
h1=diversity(c(5,5,5,5),base = 10)
hmax=log10(length(c(5,5,5,5)))
h1/hmax
h2=diversity(c(1,1,1,17), base = 10)
h2/hmax
h3=diversity(c(2,2,2,34), base = 10)
h3/hmax

#Ex4.4
x41=c(842,843,844,846,846,847,848,849)
var(x41)
sd(x41)
mean(x41)
sd(x41)/mean(x41)
cx41=c(2,3,4,6,6,7,8,9)
var(cx41)
sd(x41)
mean(cx41)
sd(cx41)/mean(x41)

x42=c(800,900,950,1100,1250,1300)
var(x42)
sd(x42)
mean(x42)
sd(x42)/mean(x42)
cx42=c(8,9,9.5,11,12.5,13)
var(cx42)
sd(x42)
mean(cx42)
sd(cx42)/mean(x42)
