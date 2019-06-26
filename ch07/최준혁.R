
# 1번.mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 통계적으로 
#유의한지 t-test를 통해 확인해 보시오.
View(mtcars)
shapiro.test(mtcars$mpg[mtcars$am==1])
shapiro.test(mtcars$mpg[mtcars$am==0])#정규성을 나타낸다
var.test(mtcars$mpg~mtcars$am)#분산이 동일하다.
t.test(mtcars$mpg~mtcars$am, var.equal=TRUE)
sd(mtcars$mpg[mtcars$am==1])
sd(mtcars$mpg[mtcars$am==0])
cat('자동차 기어 종류에 따른 mpg의 차이가 통계적으로 유의한지 알아보기 위해 표본 추출을 
통해 auto:13, manual:19 항목의 mpg를 측정한 결과 manual의 mpg는 17.14737 +- 6.166504,
auto의 mpg는 24.39231 +- 3.833966으로 나타났습니다. 이를 유의수준 0.05에서 가설검정
하면 검정통계량과 유의 확률이 -4.1061(p-value = 0.000285)으로 나타나 두 기어종류의 mpg의 
차이가 통계적으로 유의하지 않은 것으로 판단됩니다.')

# 2. MASS 패키지에 내장된 Cars93 데이터프레임에 대해서 생산국(Origin)이 USA vs. non-USA 
# 2개의 group 에 대해서 차 가격(Price)의 평균이 차이가 있는지를검정해보시오.
library(MASS)
shapiro.test(Cars93$Price[Cars93$Origin=="non-USA"])
shapiro.test(Cars93$Price[Cars93$Origin=="USA"])#정규성을 띄지않는다.
var.test(Cars93$Price~Cars93$Origin)#분산이 동일하지 않다.
t.test(Price ~ Origin, data=Cars93, var.equal=FALSE)
sd(Cars93$Price[Cars93$Origin=="non-USA"])
sd(Cars93$Price[Cars93$Origin=="USA"])
cat('생산국에 따른 가격의 평균 차이가 있는지 알아보기 위해 표본 추출을 통해 USA:48,
NON-USA:45 항목의 가격을 조사한 결과 USA의 Price는 18.57292 +- 7.816914, Non-USA의
Price는 20.50889 +- 11.30675로 나타났습니다. 이를 유의수준 0.05에서 가설검정 하면
검정통계량과 유의 확률이 -0.96555(p-value = 0.3368)으로 나타나 두 표본의 평균의 차이가
있으며, non-USA제품이 USA 제품보다 평균 가격이 크다는 것으로 판단됩니다.')


#3. mpg 데이터셋에서 다음을 검정해 보시오.
View(mpg)
library(dplyr)
library(ggplot2)
#1) subcompact 자동차와 midsize 자동차의 고속도로 연비
shapiro.test(mpg$hwy[mpg$class=="subcompact"])
shapiro.test(mpg$hwy[mpg$class=="midsize"])#정규성을 띄지않는다.
data1 <- mpg %>%
  filter(class %in% c("subcompact","midsize"))
var.test(data1$hwy~data1$class)#분산이 다르다.
t.test(data1$hwy~data1$class, var.equal = FALSE)
sd(mpg$hwy[mpg$class=="subcompact"])
sd(mpg$hwy[mpg$class=="midsize"])
cat('subcompact와 midsize 종류에 따른 hwy의 차이가 통계적으로 유의한지 알아보기 위해
표본 추출을 통해 subcompact:35, midsize:41 항목의 hwy를 측정한 결과 subcompact의 hwy는
28.14286 +- 5.375012, midsize의 hwy는 27.29268 +- 2.13593으로 나타났습니다. 이를 
유의수준 0.05에서 가설검정 하면 검정통계량과 유의 확률이0.93116(p-value = 0.3548)으로
나타나 두 기어종류의 hwy의 차이가 통계적으로 유의한 것으로 판단됩니다.')

#2) 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비
shapiro.test(mpg$cty[mpg$fl=="r"])
shapiro.test(mpg$cty[mpg$fl=="p"])#정규성을 띄지않는다.
data2 <- mpg %>%
  filter(fl %in% c("r","p"))
var.test(data2$cty~data2$fl)#분산이 다르다.
t.test(data2$cty~data2$fl, var.equal=FALSE)
sd(mpg$cty[mpg$fl=="r"])
sd(mpg$cty[mpg$fl=="p"])
cat('r과 p종류에 따른 cwy의 차이가 통계적으로 유의한지 알아보기 위해 표본 추출을
통해 r:168, p:52 항목의 cty를 측정한 결과 r의 cty는 16.73810 +- 3.88801, r은
17.36538 +- 3.042373으로 나타났습니다. 이를 유의수준 0.05 에서 가설검정 하면
검정통계량과 유의 확률이 -1.0662(p-value = 0.2875)으로 나타나 두 휘발류의 cty의
차이가 통계적으로 유의한 것으로 판단됩니다.')
#3) subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비
data3 <- mpg %>%
  filter(class=="subcompact" & drv %in% c("f","r"))
shapiro.test(data3$cty[data3$drv=="f"])
shapiro.test(data3$cty[data3$drv=="r"])
var.test(data3$cty~data3$drv)
t.test(data3$cty~data3$drv,var.equal=F)
sd(data3$cty[data3$drv=="f"])
sd(data3$cty[data3$drv=="r"])
cat('자동차의 구동 종류에 따른 cty의 차이가 통계적으로 유의한지 알아보기 위해 표본 추출을
통해 r:9, f:22 항목의 cty를 측정한 결과 r의 cty는 15.88889 +- 1.452966, f은 22.36364
+- 4.520277으로 나타났습니다. 이를 유의수준 0.05에서 가설검정 하면 검정통계량과 유의
확률이 --4.1727(p-value = 0.0002498)으로 나타나 두 구동 방식의 cty의 차이가 통계적으로
    유의한 것으로 판단됩니다.')

# Paired sample T 테스트

# 1. 새로운 당뇨병 치료제를 개발한 제약사에서는 치료에 지대한 영향을 주는 외부요인을
# 통제하기 위해 10명의 당뇨병 환자를 선별하여 1달 동안 '위약(placebo)'을 투여한 기간의
# 혈당 수치(Xi)와 '신약(new medicine)'을 투여한 1달 기간 동안의 혈당 수치(Yi)를 측정하여
# 짝을 이루어 혈당 차이를 유의수준 5%에서 비교하시오.

placebo <- c(51.4,52.0,45.5,54.5,52.3,50.9,52.7,50.3,53.8,53.1)
new_medicine <- c(50.1,51.5,45.9,53.1,51.8,50.3,52.0,49.9,52.5,53.0)

sd(placebo-new_medicine)
t.test(placebo,new_medicine, paired=T)

cat('새로운 당뇨병 치료제를 개발한 제약사에서는 치료에 지대한 영향을 주는 외부요인을
통제하기 위해 10명의 당뇨병 환자를 선별하여 1달 동안 위약(placebo)을 투여한 기간의 혈당
수치(Xi)와 신약(new medicine)을 투여한 1달 기간 동안의 혈당 수치(Yi)를 측정하여 짝을 이루어
혈당 차의 차이가 통계적으로 유의한지 알아보기 위해 총 10명의 환자를 선별하여 측정한 결과
혈당수치가 평균 0.64 감소 하였으며, 표준 편차는 +- 0.5699903으로 나타났습니다. 또한 유의
수준 0.05에서 검정 통계량은 3.5507(p-value = 0.006209)로 나타나 신약의 효과가 있다는 통계
적으로 유의한 결과를 얻을 수 있었습니다.')
    
#2 . 두 종류의 신발 밑창의 원재료가 닳는 정도가 차이가 있는지를 검정하기 위해서 10명의 
# 소년에게 한쪽은 A라는 원재료로 만든 신발을 신기고, 다른 한쪽은 B라는 원재료로 만든 신발
# 을 신긴 후에, 일정 기간이 지난후에 신발을 수거하여 10명의 각 소년의 왼쪽 신발 밑창의 닳은
# 정도와 오른쪽 신발 밑창의 닳은 정도의 차이를 비교하여 두 종류 원재료의 재질이 다른지를
# 검정하시오.

materialA <- c(13.2,8.2,10.9,14.3,10.7,6.6,9.5,10.8,8.8,13.3)
materialB <- c(14.0,8.8,11.2,14.2,11.8,6.4,9.8,11.3,9.3,13.6)
sd(materialA-materialB)
t.test(materialA,materialB, paired=T)

cat('두 종류의 신발 밑창의 원재료가 닳는 정도가 차이가 있는지를 검정하기 위해서 10명의
소년에게 한쪽은 A라는 원재료로 만든 신발을 신기고, 다른 한쪽은 B라는 원재료로 만든 신발을
신긴 후에, 일정 기간이 지난후에 신발을 수거하여 10명의 각 소년의 왼쪽 신발 밑창의 닳은 정
도와 오른쪽 신발 밑창의 닳은 정도의 차이를 비교하여 두 원재료의 차이를 측정한 결과 평균
닳기의 차이가 0.41 이며, 표준 편차는 +- 0.3871549로 나타났습니다.또한 유의수준 0.05에서
검정 통계량은 -3.3489(p-value = 0.008539)로 나타나 재질에 차이가 있다는 통계적으로 유의한
결과를 얻을 수 있었습니다.')