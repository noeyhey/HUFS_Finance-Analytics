setwd("C:/Users/HUFS/Downloads")


data1 <- read.csv("Table1_1.csv")

head(data1)

model <- lm(wage ~ as.factor(female) + as.factor(nonwhite) + as.factor(union) 
            + education + exper, data = data1)

summary(model)

model1 <- lm(wage ~ as.factor(female)*education 
             + as.factor(nonwhite) + as.factor(union) + education + exper, 
             data = data1)
summary(model1)

model12 <- lm(wage ~ as.factor(female):education 
              + as.factor(nonwhite) + as.factor(union) + education + exper, 
              data = data1)
summary(model12)





# 시장의 충격도 더미변수로 만들어서 확인해볼 수 있음
data <- read.csv("time_series_gfc_sim.csv")
head(data)

model2 <- lm(gdp_growth ~ . - date, data=data)
summary(model2)

model3 <- lm(gdp_growth ~ interest_rate*gfc_dummy, data = data)
model4 <- lm(gdp_growth ~ interest_rate*post_gfc, data = data)

summary(model3)
summary(model4)
