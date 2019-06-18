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
