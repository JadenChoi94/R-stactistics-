#서로 대응인 모집단
#예제2. 식욕부진증 치료
install.packages('PairedData')
library(PairedData)
data(Anorexia)
data <-Anorexia
str(data)

install.packages('psych')
library(psych)
summary(data)
describe(data)

#검정통계량
n <- length(data$Prior - data$Post)
m <- mean( data$Prior - data$Post )
s <- sd (data$Prior - data$Post)
( t.t <- m/(s / sqrt(n)) )
alpha <- 0.05
qt(alpha, df=16)

pt(t.t, df=16)#p-value, 검정통계량으로부터 구한 유의확률


#위 방법보단 요 공식으로
t.test(data$Prior, data$Post, paired=T, alternative="less") #왼쪽으로 치우친

#모집단이 세 개 이상일 경우
