# 1-1. 시행횟수가 6이고 성공확률이 1/3인 이항분포에서 성공횟수가 3이 될 확률 
dbinom(3, 6, prob=1/3)
# 1-2. 평균이 170이고 표준편차가 6인 정규분포에서 상위20%되는 사람들의 키 범위
1-pnorm(0.2, 170, 6)
# 1-3. 자유도가 3인 카이제곱분포에서 누적확률이 95%일 때의 값
pchisq(0.95, 3)
# 1-4. 자유도가 2인 t-분포에서 누적확률이 0.975일 때의 값
qt(0.975, 2)
# 1-5. 표준정규분포에서 확률변수의 값이 1일 때의 누적확률

# 2. 베르누이 시행인지 판단하시오
cat("정답: 2,4,5번")
# 3. setosa 종 Sepal.Length의 모평균에 대한 95% 신뢰구간
library(dplyr)
x<-50
y<-150
p.hat<-x/y
alpha<-0.05
z<-qnorm(1-(alpha/2))
ll <- p.hat - z*sqrt(p.hat*(1-p.hat)/n)
ul <- p.hat + z*sqrt(p.hat*(1-p.hat)/n)
(c.i <- c(ll,ul))
cat('약 0.304~0.363')

# 4. 한 농구 선수가 자유투를 던지면 10번중에서 7번 성공한다고 할 때 다음을 R을 이용하여 풀이하시오
# 4-1. 10번 던져서 9번 이상 성공할 확률
1-pbinom(9,size=10,prob=7/10)
# 4-2. 10번 던질 때 5번 이상 8번 이하로 성공할 확률
pbinom(8,size=10,prob=7/10)-pbinom(5,size=10,prob=7/10)

# 5. 다음을 R을 이용하여 검정하시오
# 2006년 조사에 의하면 한국인의 1인 1일 평균 알코올 섭취량이 8.1g이다.
# 2008년 무작위로 뽑은 알코올 섭취량은 다음과 같다. 
alcol<-c(16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97)
#평균 알코올 섭취량이 달라졌다고 할 수 있는가?
# H0==달라졌다 v.s H1== 달라지지 않았다
shapiro.test(alcol) #p-value가 0.8 이므로 정규분포에 따른다고 볼수있다.
t.test(alcol,mu=8.1,alternative="two.sided") 
#p-value가 0.05보다 크지 않기때문에 귀무가설을 기각한다. 그러므로 알코올 섭취량은 달라지지 않았다고 볼 수 있다.

# 6.정규분포에서 from <=X<=to 확률을 구하는 함수
# rangenorm(from, to, mean, sd)을 작성하고 rangenorm(-1.96, 1.96, 0, 1)의 값을 구하시오.
pnorm(1.96,mean=0,sd=1)-pnorm(-1.96,mean=0,sd=1)

# 7. mpg 데이터셋에서 다음을 검정해 보시오.
# 7-1. 
# H0=subcompact 자동차와 midsize 자동차의 도시 연비가 같다.
# H1=subcompact 자동차와 midsize 자동차의 도시 연비가 다르다.
library(ggplot2)
mpg
mpg$cty[mpg$class=="subcompact"]
shapiro.test(mpg$cty[mpg$class=="subcompact"])
qqnorm(mpg$cty[mpg$class=="subcompact"]) ; qqline(mpg$cty[mpg$class=="subcompact"])
#정규성을 나타내지 않는다.
mpg$cty[mpg$class=="midsize"]
shapiro.test(mpg$cty[mpg$class=="midsize"])
qqnorm(mpg$cty[mpg$class=="midsize"]) ; qqline(mpg$cty[mpg$class=="midsize"])
#이거 또한 정규성을 나타내지 않는다. 
#만약 두집단 모두 정규성을 따른다고 가정하면
mpg1 <- mpg %>%
  filter(class %in% c("subcompact","midsize"))
var.test(mpg1$cty~mpg1$class)
#p-value의 값이 굉장히 작으므로, 유의수준 0.05 하에서 두 집단의 분산은 다르다고 결론내린다
t.test(mpg1$cty~mpg1$class,var.equal=F)
# p-value의 값이 0.06으로 유의수준 0.05보다 큰 값이다. 따라서 귀무가설을 기각하지 못하고, 이에 따라 subcompact 
# 자동차와 midsize 자동차의 도시 연비는 차이가 없다고 결론내린다.

#7-2.
# H0=일반휘발유와 고급 휘발유의 고속도로 연비가 같다.
# H1=일반휘발유와 고급 휘발유의 고속도로 연비가 다르다.
mpg$hwy[mpg$fl=="r"]
shapiro.test(mpg$hwy[mpg$fl=="r"])
qqnorm(mpg$hwy[mpg$fl=="r"]) ; qqline(mpg$hwy[mpg$fl=="r"])


mpg$hwy[mpg$fl=="p"]
shapiro.test(mpg$hwy[mpg$fl=="p"])
qqnorm(mpg$hwy[mpg$fl=="p"]) ; qqline(mpg$hwy[mpg$fl=="p"])
# 이 유의수준 0.05보다 작다. 따라서 두 집단 모두 정규분포를 따르지 않는다고 결론내린다. 이에 따라 다음과 같이
# 두가지의 경우로 나누어서 검정한다.
# 만약 두 집단의 가격이 모두 정규분포를 따른다고 가정한다면 2-sample T-test를 사용한다.
mpg2 <- mpg %>%
  filter(fl %in% c("r","p"))
var.test(mpg2$hwy~mpg2$fl) 
#검정결과, p-value의 값이 약 0.043이므로, 유의수준 0.05 하에서 두 집단의 분산은 다르다고 결론내린다. 
t.test(mpg2$hwy~mpg2$fl,var.equal=F)
# p-value의 값이 0.002로 유의수준 0.05보다 작은 값이다. 따라서 귀무가설을 기각하고, 이에 따라
# 일반휘발유와 고급 휘발유의 고속도로 연비가 다르다고 결론 내린다.

# 8. 적합도를 검정하시오
x <- c(322, 109, 99, 29)
chisq.test(x, p=c(9, 3, 3, 1)/16)
# p-value 가 유의수준 0.05보다 크다. 

# 9.'women'을 이용하여 다음을 구하시오
plot(weight~height, women)
fit<-lm(weight~height, data=women)
summary(fit)
cor.test(women$weight, women$height)
lm(weight~height+I(height^2), women)
