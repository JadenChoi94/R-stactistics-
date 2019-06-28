# 예제 9-1. 아버지와 아들 키의 공분산과 상관계수 
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header=T, stringsAsFactors = FALSE)
str(hf)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]
str(hf.son)

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum( (hf.son$Father-f.mean) * (hf.son$Height - s.mean) )#covariance! 책에 나와있는 공식참고
(cov.xy <- cov.num / (nrow(hf.son) - 1))

# R함수를 이용한 공분산
cov(hf.son$Father, hf.son$Height) 
(r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height)))
# R함수를 이용한 상관계수
cor(hf.son$Father, hf.son$Height)#공분산과 결과값이 같다.

# 그림 9-2
par(mfrow=c(1, 1), mar=c(4, 4, 1, 1))
plot(Height~Father, pch=16, data=hf.son, xlab="아버지의 키(인치)", ylab="아들의 키(인치)")
abline(lm(Height~Father, data=hf.son), col="red", lwd=2)
