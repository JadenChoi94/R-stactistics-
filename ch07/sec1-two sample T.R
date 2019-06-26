#7장. 여러 모집단의 평균 비교 검정
#7-1
#정규성 테스트
setwd('D:/Workspace/R-statistics/ch07')
data<-read.table('chapter7.txt', header = T)
data
boy<-subset(data, gender==1)
boy
girl<-subset(data, gender==2)
girl

#1.정규성 테스트
shapiro.test(boy$weight)
qqnorm(boy$weight)#라인에서 점이 떨어져 있어서 정규성을 띄지않는다.
qqline(boy$weight)

shapiro.test(girl$weight)
qqnorm(girl$weight)
qqline(girl$weight)
#for example
iriss<-subset(iris, Species=='setosa')
shapiro.test(iriss$Sepal.Length) #P-value>0.05, 정규성 있음 
qqnorm(iriss$Sepal.Length)
qqline(iriss$Sepal.Length)
shapiro.test(iriss$Petal.Width) #P-value<0.05, 정규성 없음 
qqnorm(iriss$Petal.Width)
qqline(iriss$Petal.Width)

#2.등분산성 테스트
var.test(data$weight~data$gender)

#3. 서로 독립인 두 모집단: 평균 차이검정
#two-sample T test
t.test(data$weight ~ data$gender, mu=0, alternative="less", var.equal=TRUE)#mu:평균
#여자 남자 몸무게 평균을 뺸, H0을 채택한다.

