#2.모수와 통계량
setwd('D:/Workspace/R-statistics/ch02')
ranicafe<-read.csv('cafedata.csv',stringsAsFactors = F)
str(ranicafe)
head(ranicafe)
summary(ranicafe)
dim(ranicafe)

ranicafe$Coffees<-as.numeric(ranicafe$Coffees)
sort(ranicafe$Coffees)#factor로 읽힘 
sort(ranicafe$Coffees)[1] #최소값
sort(ranicafe$Coffees, decreasing = T)
sort(ranicafe$Coffees, decreasing = T)[1] #최대값
min(ranicafe$Coffees, na.rm=T)
max(ranicafe$Coffees, na.rm=T)

#대표값
stem(ranicafe$Coffees) #최빈값
hist(ranicafe$Coffees) 
table(ranicafe$Coffees)

#평균과 중앙값
rc<-ranicafe$Coffees
length(rc)
weight<- 1/(length(rc) -1) #NA개수를 빼줘야함 
weight
sum(rc*weight, na.rm=T)#평균
mean(rc, na.rm = T)
rc[rc=max(rc, na.rm=T)]<-480
mean(rc, na.rm=T)



length(rc)
median.idx<-(1+length(rc)-1)/2
median.idx
sort(rc)[median.idx] #중앙값
rc
median(rc, na.rm = T)

#표준편차 구하기
height<-c(164, 166, 168, 170, 172, 174, 176)
(height.m<-mean(height))
height-height.m
(height.dev<-height-height.m)
sum(height.dev)

(height.dev2<-height.dev^2)
sum(height.dev2)
length(height)
variance<-sum(height.dev2)/length(height) #분산
variance
sqrt(variance)#제곱근 계산 
standard_deviation<-sqrt(variance)# 표준편차 
standard_deviation

mean(height)
var(height)
sd(height)#표본집단 n-1

# 사분위수
quantile(rc, na.rm=T)
qs<-quantile(rc, na.rm=T)
qs
qs[4]-qs[2]        #3분위수-1분위수=IQR(interquantile range)
IQR(rc, na.rm=T)
bp<-boxplot(rc, main='커피 판매량에 대한 상자도표', axes=F)

#이상치
boxplot(cars$dist)
qs<-quantile(cars$dist)
qs
iqr<-qs[4]-qs[2]
iqr
qs[4]+1.5*iqr 
upperLimit<-qs[4]+1.5*iqr 
lowerLimit<-qs[2]-1.5*iqr
lowerLimit; upperLimit
cars$dist[cars$dist>upperLimit] #이상치
cars$dist[cars$dist<lowerLimit]
