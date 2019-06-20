'''
d : 확률
p : 누적확률
q : 누적활률이 얼마일때 x 값을 구하는 것
r : 난수 생성 
'''
#이항분포
#1

#2
n<-10
p<-4/5

( dbinom(7, size=n, prob=p))

#3
n<-20
p<-1/20

pbinom(2, size=n, prob = p)
#4
n<-20
p<-1/5
pbinom(2, size=n, prob = p)

#5
5/36

#정규분포
#1
pnorm(750, mean=800, sd=40)

#2-1
(1- pnorm(20, mean=11, sd=4))
#2-2
qnorm(0.1, mean=11, sd=4)

#3
mu <- 70
sigma <- 8
pnorm(90, mean=mu, sd=sigma)-pnorm(80, mean=mu, sd=sigma)

#4
mu <- 1.5
sigma <- 2
H0<-pnorm(1, mean=mu, sd=sigma)-pnorm(0, mean=mu, sd=sigma)
H2<-pnorm(3, mean=mu, sd=sigma)-pnorm(2, mean=mu, sd=sigma)
H0+H2
