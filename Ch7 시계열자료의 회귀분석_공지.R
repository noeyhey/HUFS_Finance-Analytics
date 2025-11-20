setwd("C:/Users/HUFS/Downloads")


##Chapter7
##read data

data<- read.csv(file="Table6_1.csv")
summary(data)


model<-lm(lnconsump~lndpi+lnwealth+interest, data=data)      
summary(model)


### time plot##
data$time<-c(1947:2000)   #time variable# 


par(mfrow=c(2,2))
plot(model$residuals~model$fitted,main="residual-fitted")
plot(model$residuals~data$time,main="time plot of residual")
plot(model$residuals~data$time, type="l", main="time plot of residual type=l")
plot(model$residuals~data$time, type="b", main="time plot of residual type=b")



plot(model$residuals~data$time,type="l")
abline(h=0,col="red")
plot(model$residuals~data$time,type="b")
abline(h=0,col="red")

acf(model$residuals)


#install.packages("lmtest")
library(lmtest)
model<-lm(lnconsump~lndpi+lnwealth+interest, data=data)      
dwtest(model)


acf(model$residuals)




#with trend dummy     
data$time<-seq(1:54) 
modelwithtrend<-lm(lnconsump~lndpi+lnwealth+interest+time, data=data)      
summary(modelwithtrend)
#acf of residuals
par(mfrow=c(1,2))
acf(modelwithtrend$residuals,main="with t")
acf(model$residuals,main="without t")
#dw
dwtest(modelwithtrend)
dwtest(model)





####input X_t-1  ## 
data<- read.csv(file="Table6_1.csv")
nn=dim(data)[1]
data$dpim1<-c(NA, data$lndpi[1:nn-1])
data$wealm1<-c(NA, data$lnwealth[1:nn-1])
data$intm1<-c(NA, data$interest[1:nn-1])




model<-lm(lnconsump~lndpi+lnwealth+interest , data=data)      
summary(model)
acf(model$residuals)


modelwithm1<-lm(lnconsump~lndpi+lnwealth+interest + dpim1+wealm1+intm1, data=data)      
summary(modelwithm1)

modelwithm1<-lm(lnconsump~lndpi+lnwealth+wealm1+intm1, data=data)      
summary(modelwithm1)


acf(modelwithm1$residuals)




par(mfrow=c(1,2))
plot(modelwithm1$residuals,main="with LCm1", type="b")
abline(h=0)
plot(model$residuals,main="without LCm1", type="b")
abline(h=0)



par(mfrow=c(1,2))
acf(modelwithm1$residuals,main="with LCm1")
acf(model$residuals,main="without LCm1")



dwtest(model)
dwtest(modelwithm1)

## witn Y_t-1## 
data$LCm1<-c(NA, data$lnconsump[1:nn-1])

modelwithLCm1<-lm(lnconsump~lndpi+lnwealth+interest+LCm1, data=data)      
summary(modelwithLCm1)


par(mfrow=c(1,2))
acf(modelwithLCm1$residuals,main="with LCm1")
acf(model$residuals,main="without LCm1")

dwtest(model)
dwtest(modelwithLCm1)










###   replacement has 53 rows, data has 54
##DIFF# 
diffentiateew <- data$lnconsump[2:dim(data)[1]] - data$lnconsump[1:dim(data)[1]-1]   
ynew <- diff(data$lnconsump, difference=1)  


# data$ynewww <- ynew
data$ynew<-c(NA,ynew) 




acf(data$lnconsump)

par(mfrow=c(1,1))

ynew<-diff(data$lnconsump, difference=1) 
lndpinew<-diff(data$lndpi, difference=1)
lnwealthnew<-diff(data$lnwealth, difference=1)
interestnew<- diff(data$interest, difference=1)


modelwithdiff<- lm(ynew~lndpinew+lnwealthnew+interestnew) 
summary(modelwithdiff)


modelwithdiff<- lm(ynew~lndpinew+lnwealthnew+data$interest[2:54]-1) 
summary(modelwithdiff)

modelwithdiff<-lm( dlnconsump~ dlndpi+dlnwealth +interest-1,data=data)
summary(modelwithdiff)





plot(modelwithdiff$residuals~data$year[2:54],type="b")

acf(modelwithdiff$residuals,main="diff")
acf(model$residuals,main="no diff")

library(lmtest)
dwtest(modelwithdiff)
dwtest(model)



cor(data$dlndpi[2:54], data$dlnwealth[2:54])

cor(data$dlndpi[2:54], data$interest[2:54])


cor(data$dlnwealth[2:54], data$interest[2:54])

install.packages("car")
library(car)
vif(modelwithdiff)



