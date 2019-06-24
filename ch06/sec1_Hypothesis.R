setwd('D:/Workspace/R-statistics/ch06')
data <- read.csv('2016.6th.csv', header=T)
str(data)
tmp <- subset(data, data$나이==7 )
height.p <- tmp$X104.키

set.seed(9)
height <- height.p[sample(length(height.p), 15)]
height

mean(height)
mu0 <- 1220
sd(height)
sqrt( length(height) )
(mean(height) - mu0) / (sd(height) / sqrt(length(height)))

pt(0.727, 14)
