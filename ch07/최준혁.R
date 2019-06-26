
# 1번.mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 통계적으로 
#유의한지 t-test를 통해 확인해 보시오.
View(mtcars)
t.test(mtcars$am,mtcars$mpg, alternative="less", var.equal=TRUE)


# 2번.MASS 패키지에 내장된 Cars93 데이터프레임에 대해서 생산국(Origin)이 USA vs. non-USA 
#2개의 group 에 대해서 차 가격(Price)의 평균이 차이가 있는지를검정해보시오.
#H0 : USA와 non-USA의 가격이 같다
#H1 : USA와 non-USA의 가격이 다르다
 t.test(Price ~ Origin, data=Cars93, alternative="two.sided", var.equal=FALSE)
#p-value 가 0.3 이므로 USA와 non-USA의 가격의 평균 차이가 없다는 것으로 판단됩니다.



#3. mpg 데이터셋에서 다음을 검정해 보시오.
View(mpg)

#1) subcompact 자동차와 midsize 자동차의 고속도로 연비
sub<-subset(mpg, class=='subcompact')
mid<-subset(mpg, class=='midsize')
t.test(sub$hwy, mid$hwy, alternative ="two.sided", var.equal = FALSE)


#2) 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비
normal<-subset(mpg, fl=='r')
gas<-subset(mpg, fl=='p')
t.test(normal$cty, gas$cty, alternative="two.sided", var.equal=FALSE)

#3) subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비
sub1<-subset(sub, drv=='f')
sub2<-subset(sub, drv=='r')
t.test(sub1$cty, sub2$cty, alternative="two.sided", var.equal=FALSE)

