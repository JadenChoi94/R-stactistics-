#3. 확률과 확률분포
install.packages('prob')
library(prob)
tosscoin(1)
rolldie(1)
urnsamples(1:3, size=2)
urnsamples(1:3, size=2, replace=T)#구슬을 하나뽑고 다시 집어넣음 #복원추출
urnsamples(c(rep('R', 3), rep('B',2)), size=2)
tosscoin(2, makespace = T) #확률표시해줌 

#예제 3-2, 확률변수의 평균과 기댓값
x<-c(0,1,2)
px<-c(1/4, 2/4, 1/4)
ex<-sum(x*px)
ex
x2<-x^2
x2
ex2<-sum(x2*px)#X제곱의 기댓값
VARX<-ex2-ex^2 #E(X^2)-E(X)^2
VARX

