#Polynomial Regression
women

#신장에 따른 몸무게
plot(weight~height, women)
fit<-lm(weight~height, women)
abline(fit, col='red', lwd=2)

summary(fit)
cor.test(women$weight, women$height)

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

fit2<-lm(weight~height+I(height^2), women)
plot(weight~height, women)
lines(women$height, fitted(fit2), col='green', lwd=2)

summary(fit2)
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))

fit3<-lm(weight~height+I(height^2)+I(height^3), women)
plot(weight~height, women)
lines(women$height, fitted(fit3), col='orange', lwd=2)

summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(1,1))

#Adj R, plot 로 판별

#Akaike information criteria
AIC(fit2)
AIC(fit3)

#Overfit
#Polynomial
lm(weight~height+I(height^2), women)