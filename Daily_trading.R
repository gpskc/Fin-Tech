install.packages("TTR")
install.packages("quantmod")
install.packages("reticulate")
install.packages("PerformanceAnalytics")

library(TTR)
library(quantmod)
library(reticulate)
library(PerformanceAnalytics)

setwd("E:/UTD/Fall/FIN 6392/R Codes")
use_python("E:/UTD/Fall/FIN 6392/R Codes",required=T)
rm(list=ls())

##### DAILY TRADING DATA #####
sym.vec <- c("T","XOM","JCP","LUV","THC")
getSymbols(sym.vec,from="2014-02-12",to="2019-04-01")

# AT&T #
names(T) <- c("Open", "High","Low","Close","Volume","Adjusted")
T$logReturn <- CalculateReturns(T[,"Adjusted",drop=F],method="log")
T <- data.frame(Date=as.Date(index(T)),coredata(T))
T <- cbind(Company='AT&T',Ticker='T',T)
SMA20 <- SMA(T[,"Close"],n=20)
EMA14 <- EMA(T[,"Close"],n=14)
BB20 <- BBands(T[,"Close"],sd=2.0)
RSI14 <- RSI(T[,"Close"],n=14)
MACD <- MACD(T[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
T <- data.frame(T,SMA20,EMA14,BB20,RSI14,MACD); T <- na.omit(T)

# Exxon Mobil #
names(XOM) <- c("Open", "High","Low","Close","Volume","Adjusted")
XOM$logReturn <- CalculateReturns(XOM[,"Adjusted",drop=F],method="log")
XOM <- data.frame(Date=as.Date(index(XOM)),coredata(XOM))
XOM <- cbind(Company='Exxon Mobil',Ticker='XOM',XOM)
SMA20 <- SMA(XOM[,"Close"],n=20)
EMA14 <- EMA(XOM[,"Close"],n=14)
BB20 <- BBands(XOM[,"Close"],sd=2.0)
RSI14 <- RSI(XOM[,"Close"],n=14)
MACD <- MACD(XOM[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
XOM <- data.frame(XOM,SMA20,EMA14,BB20,RSI14,MACD); XOM <- na.omit(XOM)

# JC Penny #
names(JCP) <- c("Open", "High","Low","Close","Volume","Adjusted")
JCP$logReturn <- CalculateReturns(JCP[,"Adjusted",drop=F],method="log")
JCP <- data.frame(Date=as.Date(index(JCP)),coredata(JCP))
JCP <- cbind(Company='JC Penny',Ticker='JCP',JCP)
SMA20 <- SMA(JCP[,"Close"],n=20)
EMA14 <- EMA(JCP[,"Close"],n=14)
BB20 <- BBands(JCP[,"Close"],sd=2.0)
RSI14 <- RSI(JCP[,"Close"],n=14)
MACD <- MACD(JCP[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
JCP <- data.frame(JCP,SMA20,EMA14,BB20,RSI14,MACD); JCP <- na.omit(JCP)

# Southwest Airlines #
names(LUV) <- c("Open", "High","Low","Close","Volume","Adjusted")
LUV$logReturn <- CalculateReturns(LUV[,"Adjusted",drop=F],method="log")
LUV <- data.frame(Date=as.Date(index(LUV)),coredata(LUV))
LUV <- cbind(Company='Southwest Airlines',Ticker='LUV',LUV)
SMA20 <- SMA(LUV[,"Close"],n=20)
EMA14 <- EMA(LUV[,"Close"],n=14)
BB20 <- BBands(LUV[,"Close"],sd=2.0)
RSI14 <- RSI(LUV[,"Close"],n=14)
MACD <- MACD(LUV[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
LUV <- data.frame(LUV,SMA20,EMA14,BB20,RSI14,MACD); LUV <- na.omit(LUV)

# Tenet Healtcare #
names(THC) <- c("Open", "High","Low","Close","Volume","Adjusted")
THC$logReturn <- CalculateReturns(THC[,"Adjusted",drop=F],method="log")
THC <- data.frame(Date=as.Date(index(THC)),coredata(THC))
THC <- cbind(Company='Tenet Healthcare',Ticker='THC',THC)
SMA20 <- SMA(THC[,"Close"],n=20)
EMA14 <- EMA(THC[,"Close"],n=14)
BB20 <- BBands(THC[,"Close"],sd=2.0)
RSI14 <- RSI(THC[,"Close"],n=14)
MACD <- MACD(THC[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
THC <- data.frame(THC,SMA20,EMA14,BB20,RSI14,MACD); THC <- na.omit(THC)

daily_data <- rbind(T,XOM,JCP,LUV,THC)
head(daily_data,50)
tail(daily_data,50)

##### QUARTERLY FUNDAMENTAL DATA #####
source_python("quart_funds.py")
quarterly_data <- quart_funds(tickers=sym.vec,from_date='2014-04-01',to_date='2019-04-01')
head(quarterly_data)
tail(quarterly_data)



