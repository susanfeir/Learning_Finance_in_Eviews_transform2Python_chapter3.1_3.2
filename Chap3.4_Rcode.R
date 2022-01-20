library(readxl)
library(zoo)
library(rugarch)
library(fGarch)
library(data.table)
library(forecast)
library(gridExtra)
library(ggplot2)
library(tseries)

path = file.path(file.choose()) #choose Chap3.1_3.4_data_daily.xlsx
df <- read_excel(path, sheet = "Sheet2") #日次データの読み込み
df <- data.frame(df)

#3.4.1 simple VaR
rgree <- diff(log(df$gree))#calculate the daily return of gree stock share
rmufj <- diff(log(df$mufj))#calculate the daily return of mufj stock share

sigma1=sd(rgree) #標準偏差を計算
sigma2=sd(rmufj)　#標準偏差を計算

# qnorm(0.95)#1.644854,The function qnorm() aims to find the boundary value, A in P(X < A), given the probability P.
varg=(10^7)*(sqrt(10))*1.65*sigma1　#VaRを計算
varm=(10^7)*(sqrt(10))*1.65*sigma2　#VaRを計算

# 3.4.2 IGARCH
model.arima = auto.arima(rgree , max.order = c(3 , 0 ,3) , stationary = TRUE , trace = T , ic = 'aicc')
spec1 = ugarchspec(
  variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
  mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
  distribution.model="norm", fixed.pars=list(omega=0))#IGARCH modelのパラメータを設定する

fit = ugarchfit(spec1, data = rgree)#rgreeのデータでIGARCHモデルをフィッティング
coef(fit) #推定したモデルの結果をチェック
#https://logicalerrors.wordpress.com/2017/08/14/garch-modeling-conditional-variance-useful-diagnostic-tests/

vt1 = coef(fit)[3]*sigma(fit)[732]^2+coef(fit)[2]*rgree[732]^2#一期先のボラティリティを計算
varg1=as.numeric((10^7)*(10^0.5)*1.65*sqrt(vt1))#VaRを計算
varg1



# # plot(fit,which="all")
# L <- 30
# n<-length(rgree)
# nparam<-length(coef(fit))-1
# resid <- as.numeric(residuals(fit))
# garch.box<-rep(0,L)
# garch.boxsq<-rep(0,L)
# for(i in 1:L){
#   box<-Box.test(resid,lag=i,type="Ljung-Box")
#   boxsq<-Box.test(resid^2,lag=i,type="Ljung-Box")
#   garch.box[i]<-box$p.value
#   garch.boxsq[i]<-boxsq$p.value
# }
# 
# par(mfrow=c(3,4),mar=c(4,4,4,0.5),las=1)
# plot(resid,type="l",main="Residual")
# plot(garch.box,type="b",ylim=c(0,1),main="Ljung-Box p-value")
# abline(h=0.05,lty=2,col="red")
# abline(v=2.5)
# acf(resid[nparam:n],main="ACF")
# pacf(resid[nparam:n],main="PACF")
# cpgram(resid,main="Cumulative Periodgram")
# qqnorm(resid,main="Normal QQ plot")
# qqline(resid)
# plot(resid,type="n",bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
# plot(resid^2,type="l",main="Residual Square")
# plot(garch.boxsq,type="b",ylim=c(0,1),main="Ljung-Box p-value")
# abline(h=0.05,lty=2,col="red")
# abline(v=2.5)
# acf(resid[nparam:n]^2,main="ACF")
# pacf(resid[nparam:n]^2,main="PACF")

# 3.4.3 GARCH

rt = rgree #
rt1=shift(rt,2) #収益率の二次のラグのデータを用意
new_data <- data.frame(cbind(rt,rt1))
new_data <- new_data[3:732,]#欠損値部分を削除


# qplot(rt , geom = 'histogram') + geom_histogram(fill = 'lightblue' , bins = 30) +
#   geom_histogram(aes(rt[rt < quantile(rt , 0.025)]) , fill = 'red' , bins = 30) +
#   labs(x = 'Daily Returns')
# 
# jarque.bera.test(rt)
# p2_1 = qplot(rt , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) +
#   geom_density(aes(rnorm(200000 , 0 , sd(rt))) , fill = 'red' , alpha = 0.25) +
#   labs(x = '')
# 
# p2_2 = qplot(rt , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) +
#   geom_density(aes(rnorm(200000 , 0 , sd(rt))) , fill = 'red' , alpha = 0.25) +
#   coord_cartesian(xlim = c(-0.07 , -0.02) , ylim = c(0 , 10)) +
#   geom_vline(xintercept = c(qnorm(p = c(0.01 , 0.05) , mean = mean(rt) , sd = sd(rt))) ,
#              color = c('darkgreen' , 'green') , size = 1) + labs(x = 'Daily Returns')
# grid.arrange(p2_1 , p2_2 , ncol = 1)
# ##On the figure above, 
# # Density plots are shown for stock returns (blue) and 
# # normal distributed data (red). 
# # Vertical lines of the lower plot represent 
# # the normal corresponding quantile 
# # for a = 0.05 (light green) and a = 0.01 (dark green). 
# # Lower plot indicates that for 95% significance, 
# # normal distribution usage may overestimate the value at risk. 
# # However, for 99% significance level, 
# # a normal distribution would underestimate the risk.

# 
# fitdist(distribution = 'std' , x = rt)$pars
# cat("For a = 0.05 the quantile value of normal distribution is: " ,
#     qnorm(p = 0.05) , "\n" ,
#     "For a = 0.05 the quantile value of t-distribution is: " ,
#     qdist(distribution = 'std' , shape = 2.952385319  , p = 0.05) , "\n" , "\n" ,
#     'For a = 0.01 the quantile value of normal distribution is: ' ,
#     qnorm(p = 0.01) , "\n" ,
#     "For a = 0.01 the quantile value of t-distribution is: " ,
#     qdist(distribution = 'std' , shape = 2.952385319  , p = 0.01) , sep = "")
# ##quantiles for 95% significance level indicate that normal distribution overestimates risk but for 99% underestimation of risk occurs.

# fGarch package
m2=garchFit(~arma(2,0)+garch(1,1), data=new_data$rt, trace=F, cond.dist='std') #GARCHモデルを構築する
coef(m2)   #推定したGARCHモデルの結果をチェック
predict(m2,10)　#10期先のボラティリティを予測する
alpha <- 0.95　#テール確率5％の場合
nu2=m2@fit$coef[["shape"]]　#t分布の自由度を取得
mu2 <- sum(predict(m2,10)$meanForecast)　#hat{rt}（rtの予測値の総和）を計算
sig2 <- mean(predict(m2,10)$standardDeviation) #10期先のボラティリティを取得
var2=(mu2 +qstd(alpha,nu=nu2)*sig2)*(10^7) #10期のVaRを計算
var2


sig. <- sqrt(10)*sqrt(coef(m2)[["alpha1"]]*(predict(m2,10)$meanForecast[10])^2+ coef(m2)[["beta1"]]*(predict(m2,10)$standardDeviation[10])^2)  #10期先のボラティリティを取得
var2.=(mu2 +qstd(alpha,nu=nu2)*sig.)*(10^7) #10期のVaRを計算
var2.

# qt(alpha, df = nu2)*sqrt((nu2-2)/nu2)
# qstd(alpha,nu=nu2)
# https://stats.stackexchange.com/questions/310831/garch-estimates-differ-in-rugarch-r-vs-eviews

##backtesting
# var_=qstd(1-alpha,nu=nu2)*m2@sigma.t
# plot(new_data$rt, type = "b", main = "95% VaR Backtesting", xlab = "Date", ylab = "Return/VaR in percent")
# lines(var_, col = "red")
# legend("topright", inset=.05, c("Return","VaR"), col = c("black","red"), lty = c(1,1))
# sum(new_data$rt<var_)

# #fGarch package
# X.mat = cbind(y=new_data$rt, rt1 =new_data$rt1)
# m3=garchFit(y~garch(1,1), data=X.mat, cond.dist='std') #GARCHモデルを構築する
# summary(m3)   #推定したGARCHモデルの結果をチェック
# predict(m3,10)　#10期先のボラティリティを予測する
# alpha <- 0.95　#テール確率5％の場合、95パーセンタイル点
# nu3=m3@fit$coef[["shape"]]　#t分布の自由度を取得
# mu3 <- sum(predict(m3,10)$meanForecast)　#hat{r}_t（rtの予測値の総和）を計算
# sig3 <- predict(m3,10)$standardDeviation[10]#10期先のボラティリティを取得
# var3=(mu3 +qstd(alpha,nu=nu3)*sig3)*(10^7) #10期先のVaRを計算
# var3
# 
# 
# # # uGarch package: different result confirmed
# 
# model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)),
#                         mean.model = list(armaOrder = c(2 , 0)),
#                         distribution.model = "std",fixed.pars=list(ar1=0))
# ugarch_fit = ugarchfit(spec = model.spec, data =new_data$rt)
# ugarch_fit
# # plot(ugarch_fit, which = 2)
# forc <- ugarchforecast(ugarch_fit, n.ahead=10)
# forc
# mu <- sum(fitted(forc))
# nu=ugarch_fit@fit$coef[["shape"]]
# sig <- mean(sigma(forc))
# var1=(mu+qstd(0.95,nu=nu)*sig)*(10^7)
# var1
# 
# 
# sig1. <- sqrt(10)*sqrt(coef(ugarch_fit)[["alpha1"]]*(fitted(forc)[10])^2+ coef(ugarch_fit)[["beta1"]]*(sigma(forc)[10])^2)  #10期先のボラティリティを取得
# var1.=(mu2 +qstd(alpha,nu=nu2)*sig1.)*(10^7) #10期のVaRを計算
# var1.
#
#VaR <- as.numeric(quantile(ugarch_fit, probs = alpha))
# 
# btest <- VaRTest(1-alpha, actual = -rgree,
#                  VaR = quantile(ugarchfit(model.spec, data = -rgree), probs = 1-alpha))
# btest$expected.exceed # number of expected exceedances = (1-alpha) * n
# btest$actual.exceed
# btest$uc.H0
# btest$uc.Decision
# btest$cc.H0
# btest$cc.Decision

