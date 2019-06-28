#
data<-read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")
data$rank<-as.factor(data$rank)
str(data)

#값이 0 아니면 1, 합격, 불합격: logistic regression
train<-data[1:200, ]
test<-data[201:400, ]
#glm: general
model<-glm(admit~gre+gpa+rank, data=data, family = 'binomial')
summary(model)

model2<-glm(admit~gpa+rank, data=data, family = 'binomial')
summary(model2)
AIC(model, model2)
