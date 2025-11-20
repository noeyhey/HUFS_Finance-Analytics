setwd("C:/Users/HUFS/Downloads")

data <- read.csv("Table4_2.csv")

model <- lm(expend ~ income + wealth, data=data)
summary(model)

cor(data$income, data$wealth)
# -> 심각한 다중공선성 존재
cor(log(data$income), log(data$wealth))


par(mfrow=c(1,1))
plot(model$residuals ~ model$fitted.values)
abline(h=0, col="red")

library(car)
model.all <- lm(expend ~ income + wealth, data=data)
vif(model.all)



## variable selection
model <- lm(expend ~ income + wealth, data=data)
summary(model)

step(model, direction="both")
step(model, direction="backward")
step(model, direction="forward")
