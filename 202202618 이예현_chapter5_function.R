# chapter5. 회귀모형의 함수 형태
# setwd("C:/Users/yehye/OneDrive/바탕 화면/파이낸스어낼리틱스")

## example1. Cobb-Douglas 생산함수
data1 <- read.csv("Table2_1.csv")

### 연구 주제: 노동, 자본의 투입량과 생산량의 관계
### -> 생산을 위한 input(x) 대비 output(y)

View(data1)

model <- lm(output ~ labor + capital, data=data1)
summary(model)

plot(model$residuals ~ model$fitted)
abline(h=0, col="red")

library(lmtest)
bptest(model)
gqtest(model)

# x, y 모두 로그변환
modelloglog <- lm(lnoutput ~ lncapital, data=data1)
summary(modelloglog)

plot(modelloglog$residuals ~ modelloglog$fitted)
abline(h=0, col="red");

bptest(modelloglog)
gqtest(modelloglog)


# par(mfrow = c(1,2)) -> 그래프 창 나눠보기
# plot 파라미터 main = 제목





## example2. GDP-Growth model 경제성장함수
data2 <- read.csv("Table2_5.csv")
dim(data2)

model2 <- lm(rgdp ~ time, data=data2)
summary(model2)

# 잔차플랏에서 비선형성(곡선형태)을 확인
plot(model$residuals ~ model$fitted.values)
abline(h=0, col="red")

plot(rgdp ~ time, data=data2)
abline(model2, col="red")


# 로그 변환한 모델
modelloglin <- lm(lnrgdp ~ time, data=data2)
summary(modelloglin)

par(mfrow=c(1,2))
plot(model2$residuals ~ model2$fitted.values, main="no-trans")
abline(h=0, col="red")
plot(modelloglin$residuals ~ modelloglin$fitted.values)
abline(h=0, col="red")


# time^2 model
model_2 <- lm(rgdp ~ time2, data=data2)
summary(model_2)


plot(model2$residuals ~ data2$time)
plot(rgdp ~ time, data=data2)
abline(model2)







## example3. Engel-food expenditure model 엥겔지출함수
data3 <- read.csv("Table2_8.csv")
View(data3)

data3$sfdhoperc <- data3$sfdho*100

model3 <- lm(sfdhoperc ~ expend, data=data3)
summary(model3)

plot(sfdho ~ expend, data=data3)

plot(model3$residuals ~ model3$fitted.values)
abline(h=0, col="red")


# x 로그변환
modellinlog <- lm(sfdhoperc ~ lnexpend, data=data3)
summary(modellinlog)

plot(modellinlog$residuals ~ modellinlog$fitted.values)
abline(h=0, col="red")


bptest(modellinlog)
gqtest(modellinlog)


library(sandwich)
library(lmtest)
coeftest(modellinlog, vcov=vcovHC(modellinlog, type="HC1"))




plot(model3$residuals ~ model3$fitted.values)
abline(h=0, col="red")

plot(model3$residuals ~ model3$expend)
abline(h=0, col="red")

### y 에 로그 변환을 해야 하는 게 맞지만, % 이기 때문에 하지 않음.
