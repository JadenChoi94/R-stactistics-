#다중회귀분석
state.x77
states<-as.data.frame(state.x77[,c('Murder', 'Population','Illiteracy','Income','Frost')])
fit<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)

summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

fit1<-lm(Murder~., data=states)
summary(fit1)

fit2<-lm(Murder~Population+Illiteracy, data=states)
summary(fit2)

# Akaike information criteria
AIC(fit2, fit3)#값이 적을수록 좋은 모델

# 자동화 stepwise 
# step(fit1, direction='both')
# Backward stepwise regression, Forward stepwise regression
step(fit1, direction='backward')

fit3<-lm(Murder~1, states)
step(fit3, direction='forward', 
     scope=~Population+Illiteracy+Income+Frost)
step(fit3, direction='forward', scope=list(upper=fit1, lower=fit3) )

install.packages('leaps')
library(leaps)
#method=c("exhaustive", "forward", "backward", "seqrep"),
#seqrep=변수를 추가 또는 삭제하는 것을 반복
subsets<-regsubsets(Murder~., data=states, method='seqrep', nbest=4)
subsets2<-regsubsets(Murder~., data=states, method='exhaustive', nbest=2)

summary(subsets)
plot(subsets)
summary(subsets2)
plot(subsets2)
