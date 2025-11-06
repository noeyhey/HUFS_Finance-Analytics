setwd("C:/Users/HUFS/Downloads")


data <- read.csv("Table1_1.csv")
head(data)

# 시장의 충격도 더미변수로 만들어서 확인해볼 수 있음
data1 <- read.csv("time_series_gfc_sim.csv")
head(data1)


## multiple regression model ##

model <- lm(wage ~ as.factor(female) + as.factor(nonwhite) 
            + as.factor(union) 
            + education + exper, data = data)

bptest(model)
gqtest(model)

plot(model$residuals ~ model$fitted.values)
abline(h=0, col="red")


# install.packages("lmtest")

library(lmtest)

# bptest()
# gqtest()


data$logwage <- log(data$wage)

# data[, -'logwage']

View(data)
summary(data$wage) # 0 없는 거 확인인

model7 <- lm(logwage ~ as.factor(female) + as.factor(nonwhite) 
            + as.factor(union) 
            + education + exper, data = data)

summary(model7)

bptest(model7)
gqtest(model7)

plot(model7$residuals ~ model$fitted.values)
abline(h=0, col="red")


install.packages("sandwich")
library(sandwich)
coeftest(model7, vcov = vcovHC(model7, type = "HC"))
