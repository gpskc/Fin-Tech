rm(list=ls())
setwd("E:/UTD/Fall/FIN 6392/R Codes")

install.packages("TTR")
install.packages("mefa")
install.packages("quantmod")
install.packages("PerformanceAnalytics")

library(TTR)
library(mefa)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)

sym.vec <- c("^GSPC","T","LUV","XOM")
getSymbols(sym.vec,from="2014-02-12",to="2019-04-02")

##### AT&T #####

# DAILY TRADING DATA #
names(T) <- c("Open", "High","Low","Close","Volume","Adjusted")
T$logReturn <- CalculateReturns(T[,"Adjusted",drop=F],method="log")
T$MarketReturn <- CalculateReturns(GSPC[,"GSPC.Adjusted",drop=F],method="log")
T <- data.frame(Date=as.Date(index(T)),coredata(T))
T <- cbind(Company='AT&T',Ticker='T',T,logReturn_future=lead(T$logReturn))
SMA20 <- SMA(T[,"Close"],n=20)
EMA14 <- EMA(T[,"Close"],n=14)
RSI14 <- RSI(T[,"Close"],n=14)
MACD <- MACD(T[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
T <- data.frame(T,SMA20,EMA14,RSI14,MACD)
T <- na.omit(T); rownames(T) <- NULL

# QUARTERLY FUNDAMENTAL DATA #
t <- read.csv('http://www.stockpup.com/data/T_quarterly_financial_data.csv')
names(t) <- gsub(" ",".",names(t))
t$Quarter.end <- as.Date(t$Quarter.end,origin="1899-12-30")
t <- head(t,20)
t <- cbind(Ticker2='T',t)
t <- rep(t,64)
t <- t[order(t$Ticker2,t$Quarter.end),]
rownames(t) <- NULL
t <- t[-c(64,254,255,256,320,510,511,512,704,767,768,832,896,960,1022,1023,1024,1152,1216,1278,1279,1280),]
rownames(t) <- NULL
t <- t[c("Revenue","EPS.basic","Dividend.per.share","Capital.expenditures","ROE","ROA",
         "Dividend.payout.ratio","Long.term.debt.to.equity.ratio","Equity.to.assets.ratio",
         "P.B.ratio","P.E.ratio","Net.margin","Free.cash.flow.per.share")]

# ALPHA MODEL #
att <- cbind(T,t)
write.csv(att,file="T.csv")
att <- read.csv(file="T.csv")

att_model1 <- lm(logReturn ~ MarketReturn, data=att); summary(att_model1)
att_model2 <- lm(logReturn_future ~ Close + logReturn + SMA20 + EMA14 + RSI14 + macd + signal + P.B.ratio + P.E.ratio + ROE + ROA
                                  + Dividend.payout.ratio + Long.term.debt.to.equity.ratio + Equity.to.assets.ratio, data=att); summary(att_model2)

##### Southwest Airlines #####

# DAILY TRADING DATA #
names(LUV) <- c("Open", "High","Low","Close","Volume","Adjusted")
LUV$logReturn <- CalculateReturns(LUV[,"Adjusted",drop=F],method="log")
LUV$MarketReturn <- CalculateReturns(GSPC[,"GSPC.Adjusted",drop=F],method="log")
LUV <- data.frame(Date=as.Date(index(LUV)),coredata(LUV))
LUV <- cbind(Company='Southwest Air',Ticker='LUV',LUV,logReturn_future=lead(LUV$logReturn))
SMA20 <- SMA(LUV[,"Close"],n=20)
EMA14 <- EMA(LUV[,"Close"],n=14)
RSI14 <- RSI(LUV[,"Close"],n=14)
MACD <- MACD(LUV[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
LUV <- data.frame(LUV,SMA20,EMA14,RSI14,MACD)
LUV <- na.omit(LUV); rownames(LUV) <- NULL

# QUARTERLY FUNDAMENTAL DATA #
luv <- read.csv('http://www.stockpup.com/data/LUV_quarterly_financial_data.csv')
names(luv) <- gsub(" ",".",names(luv))
luv$Quarter.end <- as.Date(luv$Quarter.end,origin="1899-12-30")
luv <- luv[-c(1),]
rownames(luv) <- NULL
luv <- head(luv,20)
luv <- rep(luv,64)
luv <- luv[order(luv$Quarter.end),]
rownames(luv) <- NULL
luv <- luv[-c(64,254,255,256,320,510,511,512,704,767,768,832,896,960,1022,1023,1024,1152,1216,1278,1279,1280),]
rownames(luv) <- NULL
luv <- luv[c("Revenue","EPS.basic","Dividend.per.share","Capital.expenditures","ROE","ROA",
             "Dividend.payout.ratio","Long.term.debt.to.equity.ratio","Equity.to.assets.ratio",
             "P.B.ratio","P.E.ratio","Net.margin","Free.cash.flow.per.share")]

# ALPHA MODEL #
southwest <- cbind(LUV,luv)
write.csv(southwest,file="LUV.csv")
southwest <- read.csv(file="LUV.csv")

sw_model1 <- lm(logReturn ~ MarketReturn, data=southwest); summary(sw_model1)
sw_model2 <- lm(logReturn_future ~ Close + logReturn + SMA20 + EMA14 + RSI14 + macd + signal + P.B.ratio + P.E.ratio + ROE + ROA
                                 + Dividend.payout.ratio + Long.term.debt.to.equity.ratio + Equity.to.assets.ratio, data=southwest); summary(sw_model2)

##### Exxon Mobil #####

# DAILY TRADING DATA #
names(XOM) <- c("Open", "High","Low","Close","Volume","Adjusted")
XOM$logReturn <- CalculateReturns(XOM[,"Adjusted",drop=F],method="log")
XOM$MarketReturn <- CalculateReturns(GSPC[,"GSPC.Adjusted",drop=F],method="log")
XOM <- data.frame(Date=as.Date(index(XOM)),coredata(XOM))
XOM <- cbind(Company='Exxon Mobil',Ticker='XOM',XOM,logReturn_future=lead(XOM$logReturn))
SMA20 <- SMA(XOM[,"Close"],n=20)
EMA14 <- EMA(XOM[,"Close"],n=14)
RSI14 <- RSI(XOM[,"Close"],n=14)
MACD <- MACD(XOM[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
XOM <- data.frame(XOM,SMA20,EMA14,RSI14,MACD)
XOM <- na.omit(XOM); rownames(XOM) <- NULL

# QUARTERLY FUNDAMENTAL DATA #
xom <- read.csv('http://www.stockpup.com/data/XOM_quarterly_financial_data.csv')
names(xom) <- gsub(" ",".",names(xom))
xom$Quarter.end <- as.Date(xom$Quarter.end,origin="1899-12-30")
xom <- head(xom,20)
xom <- rep(xom,64)
xom <- xom[order(xom$Quarter.end),]
rownames(xom) <- NULL
xom <- xom[-c(64,254,255,256,320,510,511,512,704,767,768,832,896,960,1022,1023,1024,1152,1216,1278,1279,1280),]
rownames(xom) <- NULL
xom <- xom[c("Revenue","EPS.basic","Dividend.per.share","Capital.expenditures","ROE","ROA",
             "Dividend.payout.ratio","Long.term.debt.to.equity.ratio","Equity.to.assets.ratio",
             "P.B.ratio","P.E.ratio","Net.margin","Free.cash.flow.per.share")]

# APLHA MODEL #
exxon <- cbind(XOM,xom)
write.csv(exxon,file="XOM.csv")
exxon <- read.csv(file="XOM.csv")

exxon_model1 <- lm(logReturn ~ MarketReturn, data=exxon); summary(exxon_model1)
exxon_model2 <- lm(logReturn_future ~ Close + logReturn + SMA20 + EMA14 + RSI14 + macd + signal + P.B.ratio + P.E.ratio + ROE + ROA
                                    + Dividend.payout.ratio + Long.term.debt.to.equity.ratio + Equity.to.assets.ratio, data=exxon); summary(exxon_model2)

### Performance Analysis
returns <- cbind(a=Tchart$logReturn,b=LUVchart$logReturn,c=XOMchart$logReturn)
names(returns) <- c("Treturn","LUVreturn","XOMreturn");returns <- returns[-1,]

sd.T <-sd(returns$Treturn)*sqrt(252) # 0.1751
sd.LUV <-sd(returns$LUVreturn)*sqrt(252) # 0.2872
sd.XOM <-sd(returns$XOMreturn)*sqrt(252) # 0.1868

ret.cov1 <-cov(returns$Treturn,returns$LUVreturn)*252 # 0.0104
ret.cov2 <-cov(returns$Treturn,returns$XOMreturn)*252 # 0.0121
ret.cov3 <-cov(returns$XOMreturn,returns$LUVreturn)*252 # 0.0101

wgt.T = 0.20; wgt.LUV=0.30; wgt.XOM=0.50 #asset allocation

# Three Asset Portfolio Risk
#??2= w12??12 + w22??22 + w32??32+ 2w1w2??1??2??1,2+ 2w2w3??2??3??2,3+ 2w1w3??1??3??1,3
#??2= w12??12 + w22??22 + w32??32+ 2w1w2COV(1,2)+ 2w2w3COV(2,3)+ 2w1w3COV(1,3)

port.var <- wgt.T^2*sd.T^2 + wgt.LUV^2*sd.LUV^2 + wgt.XOM^2*sd.XOM^2 + 2*ret.cov1*wgt.T*wgt.LUV + 2*ret.cov2*wgt.T*wgt.XOM + 2*ret.cov3*wgt.XOM*wgt.LUV
# portfolio variance = 0.0241

port.sd<-sqrt(port.var) #portfolio risk = 0.155

#Value-at-risk measure VaR and expected shortfall ES - the expected loss if the losses exceed VaR
#manual calculation VaR and ES

#T
T.mean <-mean(returns$Treturn)
T.risk <-sd(returns$Treturn)
VaR05.Gaussian.T <--(T.mean+T.risk*qnorm(0.05))*10000 #5% chance of losing $179.64 over the next day with $10K position
ES05.Gaussian.T <-10000*(T.mean+T.risk*dnorm(qnorm(0.05))/0.05) #5% chance the losses exceed VaR, and when it ocurs, on average, thenloss is $229.403
#LUV
LUV.mean <-mean(returns$LUVreturn)
LUV.risk <-sd(returns$LUVreturn)
VaR05.Gaussian.LUV <--(LUV.mean+LUV.risk*qnorm(0.05))*10000 #5% chance of losing $290.2 over the next day with $10K position
ES05.Gaussian.LUV <-10000*(LUV.mean+LUV.risk*dnorm(qnorm(0.05))/0.05) #5% chance the losses exceed VaR, and when it ocurs, on average, thenloss is $380.5
#XOM
XOM.mean <-mean(returns$XOMreturn)
XOM.risk <-sd(returns$XOMreturn)
VaR05.Gaussian.XOM <--(XOM.mean+XOM.risk*qnorm(0.05))*10000 #5% chance of losing $193.05 over the next day with $10K position
ES05.Gaussian.XOM <-10000*(XOM.mean+XOM.risk*dnorm(qnorm(0.05))/0.05) #5% chance the losses exceed VaR, and when it ocurs, on average, thenloss is $243.31

#T
Return.annualized(returns$Treturn) # 0.03122413
maxDrawdown(returns$Treturn) # 0.3235255
SharpeRatio.annualized(returns$Treturn) # 0.178284
SharpeRatio(returns$Treturn)
VaR(returns$Treturn, 0.05,method="gaussian") # -0.01795672
KellyRatio(returns$Treturn) # 0.7527922

#LUV
Return.annualized(returns$LUVreturn) # 0.1547738
maxDrawdown(returns$LUVreturn) # 0.3478414
SharpeRatio.annualized(returns$LUVreturn) # 0.5389614
SharpeRatio(returns$LUVreturn)
VaR(returns$LUVreturn, 0.05,method="gaussian") # -0.02900824
KellyRatio(returns$LUVreturn) # 1.124076

#XOM
Return.annualized(returns$XOMreturn) # -0.003788474
maxDrawdown(returns$XOMreturn) # 0.3300986
SharpeRatio.annualized(returns$XOMreturn) # -0.02027659
SharpeRatio(returns$XOMreturn)
VaR(returns$XOMreturn, 0.05,method="gaussian") # -0.01929786
KellyRatio(returns$XOMreturn) # 0.1957978

# the 2nd argument is the benchmark
InformationRatio(returns$LUVreturn,returns$Treturn,scale=252) # 0.4066233
InformationRatio(returns$XOMreturn,returns$Treturn,scale=252) # -0.1723035
InformationRatio(returns$Treturn,returns$LUVreturn,scale=252) # -0.4066233
InformationRatio(returns$XOMreturn,returns$LUVreturn,scale=252) # -0.5086327
InformationRatio(returns$Treturn,returns$XOMreturn,scale=252) # 0.1723035
InformationRatio(returns$LUVreturn,returns$XOMreturn,scale=252) # 0.5086327

chart.VaRSensitivity(returns$Treturn) #chart includes VaR, ES
chart.VaRSensitivity(returns$LUVreturn)
chart.VaRSensitivity(returns$XOMreturn)
charts.PerformanceSummary(returns$Treturn,methods="GaussianES")
charts.PerformanceSummary(returns$LUVreturn,methods="GaussianES")
charts.PerformanceSummary(returns$XOMreturn,methods="GaussianES")