##quantmod# 
library(quantmod)
getSymbols("^GSPC",src="yahoo")

ss <- getSymbols(Symbols="^GSPC", 
                 src = "yahoo", 
                 from= "2021-01-01", 
                 to = "2025-11-10", auto.assign = FALSE, periodicity = 'monthly')
plot(ss)

sp500<-ss$GSPC.Adjusted
lnsp500<-log(sp500)
diffsp500<-diff(sp500,difference=1)
lndiffsp500<-diff(lnsp500,difference=1)



par(mfrow=c(2,2))
plot(sp500,type="l", main="raw")
plot(lnsp500,type="l", main="log")
plot(diffsp500,type="l", main="difference without log")
plot(lndiffsp500,type="l", main="difference after log")

