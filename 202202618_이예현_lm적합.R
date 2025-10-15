setwd("C:/Users/yehye/OneDrive/바탕 화면/파이낸스어낼리틱스/")
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

### 모수 모형에서 intercept = 0 vs  ≠ 0
### ols (s.e., t-value, p-value)
### t-vlaue = (b_k - 0) / s.e(b_k) = b_k / s.e(b_k)
### -0.333 = -1.532 / 4.603
## 자유도 n-k (38 degrees (귀무가설이 0일 때 계산된 자유도))
### IT 에 대한 P-value = 0.741 (1.1e-05)
### H0 이 0일 때 인터셉트는 관찰하기 쉬움(H0 기각), B0는 0임.
### IT 는 H0 기각, B1 은 0이 아님.
##########
# 인터셉트 빼주는 모델
model_iroed1 <- lm(ROE~IT-1, data=iroed)
summary(model_iroed1)
## y_i = 0.037 X_i + e_i
## ROE 를 설명하는 데 유용한 유일한 독립변수 = IT
#### degrees = 39 (n-k, 이제 1개가 됨.)
### 적합도 R^2 = 0.7405, 74%
