#모비율 추정 연습문제
#1
set.seed(9)
n <- 10
x <- 1:100
y <- seq(-3, 3, by=0.01)

smps <- matrix(rnorm(n * length(x)), ncol=n)

xbar <- apply(smps, 1, mean)
se <- 1 / sqrt(10)
alpha <- 0.05
p <- qnorm(1 - alpha/2)
ll <- xbar - p * se
ul <- xbar + p * se

par(mar=c(5,5,4,2))
plot(y, type="n", xlab="표본추출", ylab="P", xlim=c(1, 100), ylim=c(-1.5, 1.5), cex.lab=1.8)
abline(h=0, col="red", lwd=2, lty=2)
l.c <- rep(NA, length(x))
l.c <- ifelse(ll * ul > 0, "red", "black")
arrows(1:length(x), ll, 1:length(x), ul, code=3, angle=90, length=0.02, col=l.c, lwd=1.5)

#2
ci.t <- function(x, alpha=0.05) {
  n <- length(smp)
  m <- mean(x)
  s <- sd(x)
  t <- qt(1-(alpha/2), df=n-1)
  ll <- m - t * (s / sqrt(n))
  ul <- m + t * (s / sqrt(n))
  ci <- c(1-alpha, ll, m, ul)
  names(ci) <- c("Confidence Level", "Lower limit", "Mean", "Upper limit")
  return( ci )
}
smp <- c(1:100)
ci.t(smp)
#3
ci.t <- function(x, alpha=0.05) {
  n <- length(nsmp)
  m <- mean(x)
  s <- sd(x)
  t <- qt(1-(alpha/2), df=n-1)
  ll <- m - t * (s / sqrt(n))
  ul <- m + t * (s / sqrt(n))
  ci <- c(1-alpha, ll, m, ul)
  names(ci) <- c("Confidence Level", "Lower limit", "Mean", "Upper limit")
  return( ci )
}
nsmp <- c(1:1000)
ci.t(nsmp, 0.1)

#1-sample t test 
#4
battery<-c(980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017)
t.test(battery, mu=1000, alternative="two.sided")
#p-value가 0.05보다 크기때문에 대립가설 채택, 즉 샘플이 모집단과 같지않다.

#5
score<-c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
t.test(score, mu=55, alternative="greater")
#p-value가 0.05보다 크기때문에 학생들의 성적은 올랐다고 할 수 있다.

#6
alco<-c(15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97)
t.test(alco, mu=8.1, alternative="two.sided")
#p-value = 0.5301 이므로 대립가설 채택, 즉 평균 알코올 섭취량이 달라졌다.