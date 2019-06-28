#회귀분석(regression analysis)
#예제9-2. 아버지와 아들 키 자료로부터 회귀계수 추정  
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header=T, stringsAsFactors = FALSE)
plot(hf$Height, hf$Father)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Father - mean.x)*(hf.son$Height - mean.y))
sxx <- sum((hf.son$Father - mean.x)^2)

( b1 <- sxy / sxx )
( b0 <- mean.y - b1 * mean.x )

# lm() 함수 이용 //linear model
out<-lm(Height ~ Father, data=hf.son)#앞이 y, 뒤가 x
summary(out)
#intercept:절편, father: 계수
#R-squared: 15% 설명력?


