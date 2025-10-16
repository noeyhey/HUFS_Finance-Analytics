setwd("C:/Users/HUFS/Downloads/[Ch3.실습자료]_files")
data1<-read.csv("GDP.csv")
data2<-read.csv("college.csv")
data3<-read.csv("Table1_1.csv")
iroed <- read.csv("ITroe.csv")

names(data3)
list(data3)

# 수업

model<-lm(LIFE~GDP, data=data1)
summary(model)

model12<-lm(Earnings~Cost+Grad+Debt, data=data2)
summary(model12)


# 1. ROE-IT
iroed <- read.csv("ITroe.csv")
head(iroed)
dim(iroed)
## EDA
cor(iroed$ROE, iroed$IT) # 0.6xx 선형성 약간 있음
plot(ROE~IT, data=iroed) 
### 모양은 제대로 볼 수 없음 (딱히 선형은 X)
### IT 낮은 쪽과 높은 쪽을 나눠서 봐야 하지 않을까 하는 생각을 해볼 수 있음.

model_iroed <- lm(ROE~IT, data=iroed)
summary(model_iroed)
### 잔차가 -37 ~ +29까지
### y_p = -1.53 + 0.04X1 + e1
### -> x = 사원, y = it 자본 (단위: 만 원)
##### 사원이 없을 때 it 자본 = -1.53 (해석이 안될 수 있음)
##### 사원 1인당 투자금이 만 원이 늘어날수록 ROE가 0.04 증가


plot(wage~education, data=data3)
plot(wage~age, data=data3)
plot(wage~exper, data=data3)

# 임금에 영향을 주는 요인
## X : education, age, exper experience 中 2개
## y : wage
model_wage <- lm(wage~education+age, data=data3)
summary(model_wage)

model_perfect_cor <- lm(wage~education+age+exper, data=data3)
summary(model_perfect_cor)
##### 다중공선성이 심하지 않음을 가정(1, -1의 상관관계가 있을 경우)
##### -> 다중공선성이 심할 경우, NA 로.
cor(data3$education, data3$wage)
cor(data3$age, data3$exper) # 0.97 (사람이 만든 데이터)
# exper -> 인위적 데이터임. exper(직장경력) = age-7(유아기)-education


data1<-read.csv("GDP.csv")
data2<-read.csv("college.csv")
data3<-read.csv("Table1_1.csv")
iroed <- read.csv("ITroe.csv")

model_iroed <- lm(ROE~IT, data=iroed)
summary(model_iroed)

###