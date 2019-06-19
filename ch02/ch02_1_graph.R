seq(-3,3, length.out=60)
seq(-3,3, 0.1)
x<-seq(-3,3, 0.1)
x[1]
x<-1:7
x
factor(x, levels=c(1:7))
factor(x, levels=c(1:7), labels = c('일','월','화','수','목','금','토'), ordered=T)
name<-c('철수','영의','길동')
age<-c(21,20,31)
gender<-factor(c('M','F','M'))
people<-data.frame(name,age,gender)
people$name
people[1,]
people[,2]
#==========================================================================================
#2장 
setwd('D:/Workspace/R-statistics/ch02')
data<-read.csv('2010년 인구사항.csv', header=F, na.string=c('.'))
str(data)
data$V1<-factor(data$V1, levels=c(1,2),labels=c('남자','여자'))
data$V3<-factor(data$V3, levels=1:14,labels=c('가구주','가구주의 배우자','자녀','자녀의 배우자','가구주의 부모',
                                              '배우자의 부모','손자녀, 그 배우자','증손자녀, 그배우자','조부모',
                                              '형제자매, 그배우자','형제자매의 자녀, 그 배우자',
                                              '부모의 형제자매, 그 배우자',' 기타 친인척','그외같이사는사람'))

data$V4<-factor(data$V4, levels=1:8,labels=c('안 받았음','초등학교','중학교','고등학교','대학~4년제 미만',
                                             '대학~4년제 이상','석사과정','박사과정'))

str(data)
head(data)                
#====================================================================================================
#1. 그래프
library(ggplot2)
#산점도
cars #mile/ ft
par(mfrow=c(1,2))
plot(cars$speed, cars$dist, main='속도와 제동거리', 
     xlab='속도(mph)', ylab='제동거리(ft)', pch=1, col='red')
plot(jitter(cars$speed), jitter(cars$dist), main='속도와 제동거리', 
     xlab='속도(mph)', ylab='제동거리(ft)', pch=1, col='red')
#ggplot 에서 geom_jitter()
#선그래프
par(mfrow=c(1,1))
plot(Nile, main="Nile강의 연도별 유량 변화", xlab='연도',ylab='유량') 
plot(Nile, type='p', main="Nile강의 연도별 유량 변화", xlab='연도',ylab='유량')
plot.ts (Nile)
plot(Nile)
Nile

#중요
#time series를 dataframe 으로
df_Nile<-as.data.frame(Nile)
head(df_Nile)
year<-c(1871:1970)
df_Nile$year<-year
head(df_Nile)
ggplot(df_Nile, aes(x=year, y=x))+
  geom_line()
df_Nile<-data.frame(year=time(Nile),
                    water=as.matrix(Nile))
head(df_Nile)
ggplot(df_Nile, aes(x=year, y=water))+
  geom_line()
#막대그래프
tableV5<-table(data$V5)
tableV5
barplot(tableV5, main='출생아(남자)별 빈도',
        xlab='출생아수',ylab='빈도')
#히스토그램
hist(data$V2, main='연령별 분포', xlab='연령', ylab='빈도')
#x축 구간을  10단위로
hist(data$V2, breaks=c(seq(0,90,10)), right=F, main='연령별 분포', xlab='연령', ylab='빈도')
hist(data$V2, probability = T,breaks=c(seq(0,90,10)), right=F,
     main='연령별 분포', xlab='연령', ylab='밀도' )
#다 더하면 1
#right=F
pie(table(data$V4), main='학력수준별 비중', cex=1)
