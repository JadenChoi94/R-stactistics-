set.seed(9)
n<-1000
df<-3
a.2.mean<-rep(NA, n)
a.8.mean<-rep(NA, n)
a.32.mean<-rep(NA, n)
a.64.mean<-rep(NA, n)

for (i in 1:n){
  a.2.mean[i]<-mean(rchisq(2, df=df))
  a.8.mean[i]<-mean(rchisq(8, df=3))
  a.32.mean[i]<-mean(rchisq(32, df=3))
  a.64.mean[i]<-mean(rchisq(64, df=3))
}
options(digits=3)
c(mean(a.2.mean), sd(a.2.mean))
c(mean(a.8.mean), sd(a.8.mean))
c(mean(a.32.mean), sd(a.32.mean))
c(mean(a.64.mean), sd(a.64.mean))

m<-df
s <- sqrt(2 * df)

par(mfrow=c(2,2), oma=)
hist(a.2.mean, prob=T, ylab="", xlab="", main="표본 크기 : 2", 
      col="orange", border="red")
x1<-seq(min(a.2.mean), max(a.2.mean), length=1000)
y1<-dnorm(x=x1, mean=3, sd=s/sqrt(2))
lines(x1, y1, lty=2, lwd=2, col='blue')

hist(a.8.mean, prob=T, ylab="", xlab="", main="표본 크기 : 8", 
     col="orange", border="red")
x2<-seq(min(a.8.mean), max(a.8.mean), length=1000)
y2<-dnorm( x=x2, mean=3, sd=s/sqrt(8))
lines(x2, y2, lty=2, lwd=2, col='blue')

hist(a.32.mean, prob=T, ylab="", xlab="", main="표본 크기 : 32", 
     col="orange", border="red")
x3<-seq(min(a.32.mean), max(a.32.mean), length=1000)
y3<-dnorm( x=x3, mean=3, sd=s/sqrt(32))
lines(x3, y3, lty=2, lwd=2, col='blue')

hist(a.64.mean, prob=T, ylab="", xlab="", main="표본 크기 : 64", 
     col="orange", border="red")
x4<-seq(min(a.64.mean), max(a.64.mean), length=1000)
y4<-dnorm( x=x4, mean=3, sd=s/sqrt(64))
lines(x4, y4, lty=2, lwd=2, col='blue')

#===============================================================
set.seed(9)
n<-1000
t.2.mean<-rep(NA, n)
t.8.mean<-rep(NA, n)
t.32.mean<-rep(NA, n)
t.64.mean<-rep(NA, n)

for (i in 1:n){
  t.2.mean[i]<-mean(rt(2, df=3))
  t.8.mean[i]<-mean(rt(8, df=3))
  t.32.mean[i]<-mean(rt(32, df=3))
  t.64.mean[i]<-mean(rt(64, df=3))
}
options(digits=3)
c(mean(t.2.mean), sd(t.2.mean))
c(mean(t.8.mean), sd(t.8.mean))
c(mean(t.32.mean), sd(t.32.mean))
c(mean(t.64.mean), sd(t.64.mean))

par(mfrow=c(2,2))
hist(t.2.mean, prob=T, ylab="", xlab="", main="표본 크기 : 2", 
     col="orange", border="red")
x1<-seq(min(t.2.mean), max(t.2.mean), length=1000)
y1<-dnorm( x=x1, mean=3, sd(1/sqrt(2)))
lines(x1, y1, lty=2, lwd=2, col='blue')

hist(t.8.mean, prob=T, ylab="", xlab="", main="표본 크기 : 8", 
     col="orange", border="red")
x2<-seq(min(t.8.mean), max(t.8.mean), length=1000)
y2<-dnorm( x=x2, mean=3, sd(1/sqrt(8)))
lines(x2, y2, lty=2, lwd=2, col='blue')

hist(t.32.mean, prob=T, ylab="", xlab="", main="표본 크기 : 32", 
     col="orange", border="red")
x3<-seq(min(t.32.mean), max(t.32.mean), length=1000)
y3<-dnorm( x=x3, mean=3, sd(1/sqrt(32)))
lines(x3, y3, lty=2, lwd=2, col='blue')

hist(t.64.mean, prob=T, ylab="", xlab="", main="표본 크기 : 64", 
     col="orange", border="red")
x4<-seq(min(t.64.mean), max(t.64.mean), length=1000)
y4<-dnorm( x=x4, mean=3, sd)
lines(x4, y4, lty=2, lwd=2, col='blue')
#=================================================================
set.seed(9)
n<-1000
f.8.mean<-rep(NA, n)
f.16.mean<-rep(NA, n)
f.64.mean<-rep(NA, n)
f.256.mean<-rep(NA, n)

for (i in 1:n){
  f.8.mean[i]<-mean(rf(8, df1=3, df2=6))
  f.16.mean[i]<-mean(rf(16, df1=3, df2=6))
  f.64.mean[i]<-mean(rf(64, df1=3, df2=6))
  f.256.mean[i]<-mean(rf(256, df1=3, df2=6))
}
c(mean(f.8.mean), sd(f.8.mean))
c(mean(f.16.mean), sd(f.16.mean))
c(mean(f.64.mean), sd(f.64.mean))
c(mean(f.256.mean), sd(f.256.mean))

par(mfrow=c(2,2))
hist(f.8.mean, prob=T, ylab="", xlab="", main="표본 크기 : 8", 
     col="orange", border="red")
x1<-seq(min(f.8.mean), max(f.8.mean), length=1000)
y1<-dnorm( x=x1, mean=3, sd(4/sqrt(8)))
lines(x1, y1, lty=2, lwd=2, col='blue')

hist(f.16.mean, prob=T, ylab="", xlab="", main="표본 크기 : 16", 
     col="orange", border="red")
x2<-seq(min(f.16.mean), max(f.16.mean), length=1000)
y2<-dnorm( x=x2, mean=3, sd(1/sqrt(16)))
lines(x2, y2, lty=2, lwd=2, col='blue')

hist(f.64.mean, prob=T, ylab="", xlab="", main="표본 크기 : 64", 
     col="orange", border="red")
x3<-seq(min(f.64.mean), max(f.64.mean), length=1000)
y3<-dnorm( x=x3, mean=3, sd(1/sqrt(64)))
lines(x3, y3, lty=2, lwd=2, col='blue')

hist(f.256.mean, prob=T, ylab="", xlab="", main="표본 크기 : 256", 
     col="orange", border="red")
x4<-seq(min(f.256.mean), max(f.256.mean), length=1000)
y4<-dnorm( x=x4, mean=3, sd(1/sqrt(256)))
lines(x4, y4, lty=2, lwd=2, col='blue')