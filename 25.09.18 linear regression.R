setwd("C:/Users/HUFS/downloads")
test<-read.csv("test.csv")

head(test)

dim(test)
summary(test)

test$mean<-(test$exam+test$exam2)/2

test

mean(test$exam)

#히스토그램
hist(test$exam2, col="skyblue", xlab="기말고사", main="기말고사 histogram")

#plot -> 순서 상관있음. (x, y 축)
plot(test$exam2~test$exam)
plot(exam2~exam, data=test, pch="★")

#correlation coefficient -> 순서 상관없음.
cor(test$exam, test$exam2)


#---------------------------------------


data1<-read.csv("GDP.csv") 
# 데이터의 인종과 대륙이 한정적, 전세계로 확장은 불가능
data1
plot(LIFE~GDP, data=data1)
cor(data1$LIFE, data1$GDP)

lm(LIFE~GDP, data=data1)


## capm 은 단순선형회귀모형 (intercept 없음)
## fama and french 는 다중선형회귀모형

