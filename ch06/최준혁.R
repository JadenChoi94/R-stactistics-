#모비율 추정 연습문제
#1
phat <- 0.5
print('신뢰구간은 phat - 1.96*sqrt(phat*(1-phat)/n) 
      <= p <= phat + 1.96*sqrt(phat*(1-phat)/n) 입니다')
#2
n <- 100
sp <- 80
p <- 0.8
prop.test(80, 100, 0.8)
print('95% 신뢰구간은  0.711<= p <= 0.866 입니다')


#3
options(digits = 4)
n <- 1000
phat <- 430/1000
ll <- phat - 1.64*sqrt(phat*(1-phat)/n)
ul <- phat + 1.64*sqrt(phat*(1-phat)/n)

cat('모비율 p는 신뢰도 95% 신뢰 구간',
      ll,'<= p <=',ul,'사이에 존재합니다')

#1-sample t test 
#4
battery<-c(980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017)
t.test(battery, mu=1000, alternative="two.sided")
#p-value가 0.05보다 크기때문에 귀무가설을 기각하지 못한다. 
#샘플이 모집단과 같다.

#5
score<-c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
t.test(score, mu=55, alternative="greater")
#p-value가 0.05보다 크기때문에 귀무가설을 기각하지 못한다.
#학생들의 성적은 올랐다고 할 수 없다.

#6
alco<-c(15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97)
t.test(alco, mu=8.1)
#p-value = 0.5301 이므로 귀무가설을 기각하지 못한다.
#평균 알코올 섭취량이 달라졌다고 볼 수 없다.