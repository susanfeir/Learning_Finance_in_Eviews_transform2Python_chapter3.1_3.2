#3.2.1
library(midasr)　#MIDASモデル使うパケージ
library(readxl)
library(zoo)

#データの読み込み
path = file.path(file.choose())
df_M <- read_excel(path)　#月次データの読み込み Chap3.2_data_monthly-quarterly.xlsx
df_gdp_Q <- read_excel(path, sheet = "Sheet1")　#四半期データの読み込み

df_M <- data.frame(df_M)　
df_gdp_Q <- data.frame(df_gdp_Q)

rownames(df_M) <- df_M$Date　# set index
rownames(df_gdp_Q) <- df_gdp_Q$Date　# set index

df_M$ir <- c(NA,diff(log(df_M$jp10b))*100)　# calculate the monthly return of interest rate
df_M$rtyt <- c(NA,diff(log(df_M$toyota))*100)　# calculate the monthly return of toyota stock share

df_gdp_Q$quarter = as.yearqtr(df_gdp_Q$Date,format="%Yq%q") # 日付情報から「%year%qtr」クラスを定義
monthly = df_M$Date　#月次の日付seriesを用意する
gdp = df_gdp_Q["gdp"]　#度数変更必要の四半期のデータだけを抜く
head(gdp)　#一度データの中身をチェック
gdp_M = data.frame(Date=monthly, gdp_M=spline(gdp,method="fmm", ties = mean)$y)#四半期の[gdp]をspline functionで[fmm]という[cubic interpolation]のメソッドで月次データに度数変換して、[monthly]の日付seriesと一つのdataframeに結合
#spline functionのmethodの[fmm]のほか、https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/splinefun　に参照ください
head(gdp_M,10)　#変換後のデータの中身をチェック

########################
#match to the last value (the GDP value of Q1=Mar),which is the method used in this book.ですから、二つの欠損値NAが前に加える
gdp_M0=data.frame(gdp_M$Date,c(NA,NA,gdp_M$gdp_M[1:202]))
x1 <- c("Date", "gdp_M")　#列の名前のベクトルを用意
colnames(gdp_M0) <- x1　#列の名前を変更する
head(gdp_M0,10)　#column名前変換後のデータの中身をチェック

df_M20 = merge(df_M, gdp_M0, by="Date", all=TRUE) #四半期GDPを月次に度数変更後、さらに二つずらしたのgdpデータと月次データのrgdp,irのdf_Mと結合
head(df_M20)　#中身をチェック
plot(df_M20$Date,df_M20$gdp_M,type = "line",main = "converted Monthly GDP",ylab = 'GDP',xlab = 'Date')#度数変換後のデータを図にプロットする
#https://fred.stlouisfed.org/series/JPNRGDPEXP 2000 Jan value:around 480000

df_M20$rgdp <- c(NA,diff(log(df_M20$gdp_M))*100)　#calculate the monthly gdp growth rate
plot(df_M20$Date,df_M20$rgdp,type = "line",main = "converted Monthly GDP Growth Rate",ylab = 'GDP',xlab = 'Date')#gdp成長率のデータを図にプロットする
df10 <- df_M20[4:193,]　#dataframeの欠損値部分を捨てる
eq02 <- lm(df10$rtyt~df10$rgdp+df10$ir)#回帰
print(summary(eq02))#回帰結果をチェック
########################


#下記部分のコードは、もしmatchの方法をfirstにすると、違う結果になることをチェック
#match to the first value (the GDP value of Q1=Jan)
#Note:different match method led to different OLS result
df_M2 = merge(df_M, gdp_M, by="Date", all=TRUE)
df_M2$rgdp <- c(NA,diff(log(df_M2$gdp_M))*100)
df1 <- df_M2[2:193,]
eq01 <- lm(df1$rtyt~df1$rgdp+df1$ir)
print(summary(eq01))

#3.2.2 Rolling Regression

library(rollRegres)　#Rolling Regression必要のパケージ

rolling_result <- roll_regres(
  df1$rtyt~df1$rgdp+df1$ir, width=60L)#window sizeを60（5年間）に設定してローリング回帰を実行する
plot(df1$Date,rolling_result$coefs[,2],type = "line",
     main = "coeffecients of the GDP change rate",
     ylab = 'coeffecient of rGDP',xlab = 'Date')　#GDP成長率の感応度の推移グラフを描く
abline(h=1.7, col="blue")　#OLS推定結果の感応度の1.7をグラフに追加
plot(df1$Date,rolling_result$coefs[,3],type = "line",
     main = "coeffecients of the Interest change rate",
     ylab = 'coeffecient of ir',xlab = 'Date')　#金利変化率の感応度の推移グラフを描く
abline(h=0.09, col="blue")　#OLS推定結果の感応度の0.09をグラフに追加

#3.2.3
#下記は表3.2のMIDAS回帰のコードの書き直し
Date_M <- seq(as.Date("1990/1/1"), as.Date("2010/12/31"), 
              by = "month")#1990年から2010年までの月次日付のベクトルを用意する
n <- length(Date_M)　#月次日付の長さをnとする

set.seed(1234)　#乱数シードを指定
e <- rnorm(n, mean = 0, sd = 1)　#標準正規分布に従うｎ個の、平均値が０、標準偏差が１の乱数を生成する
x <- 2*rnorm(n,0,1)　#標準正規分布に従うｎ個の、平均値が０、標準偏差が１の、値が2倍となる乱数を生成する
z <- 3*rnorm(n,0,1)　#標準正規分布に従うｎ個の、平均値が０、標準偏差が１の、値が3倍となる乱数を生成する
b0 <- 1 #ｙを作成するための係数を指定
b1 <- 1.5 ##ｙを作成するための係数を指定
b2 <- -0.7 ##ｙを作成するための係数を指定

y <- b0+b1*x+b2*z+e　##ｙを作成する
Vdata0=data.frame(Date_M,x,z,e,y)　#Date_M,x,z,e,yをデータフレームに結合する
eq02 <- lm(y~x+z)　#ここで一度回帰して、ｚの係数が0.70768であることを確認する
print(summary(eq02))　##回帰結果から、ｚの係数が0.70768であることを確認する

Date_Q <- seq(as.Date("1990/1/1"), as.Date("2010/12/31"), 
              by = "quarter")##1990年から2010年までの四半期日付のベクトルを用意する

Vdata2=Vdata0[c("y","z")]　#y、zだけを抜く
monthlyData <- ts(Vdata2, start = c(1990, 1),frequency = 12) #HFをLFに変更するためのtime series dataを用意する
quarterly <- aggregate(monthlyData, nfrequency = 4)　##HFをLFに集約する
quarterly <- data.frame(quarterly)　#time seriesをデータフレームに
quarterly <- cbind(Date_Q,quarterly)　#LFに集約後のデータと用意した四半期日付のベクトルと結合する
eq03 <- lm(quarterly$y~quarterly$z)　#y,z単純なOLSを推定する
print(summary(eq03))##the coeffecient of z is -0.6281

#EVIEWSとRが同じ設定しても、違う乱数生成する関係で、図3.19のMIDAS回帰結果を再現するために、EVIEWSで生成した乱数のx,y,zを抜き取った
path = file.path(file.choose())#EVIEWSで生成した乱数のx,y,zがある「midas.xlsx」のファイルを選ぶ
midas_yz <- read_excel(path,sheet = "Sheet2")　#yzのデータを読み込み
midas_x <- read_excel(path,sheet = "Sheet1")　#xのデータを読み込み
x <- midas_x$x#
z <- midas_yz$z#
y <- midas_yz$y#
y <- as.matrix(y)#

eq_u1 <- lm(y ~ z+mls(x, k=0:2, m = 3,almonp))#midas回帰を実行する
#y,z:quarter x:monthly  m=3
#y,z:annually x:quartely m=4
summary(eq_u1)#midas回帰結果をチェック
#k:lags, a vector
#almonp:Almon polynomial lag

list<-list(y=y,z=z,x=x)
eq_u1 <- midas_r(y~z+mls(x,0:2,3,almonp),data=list,degree = 3,start=list(x=c(0,0,0)))
summary(eq_u1)

#下記の部分はMIDASで予測する一つの例
#Forecasts MIDAS regression given the future values of regressors

##Forecast horizon
h <- 3
##New x values,monthly
set.seed(1234)
xn <- rnorm(h*3,mean(x),sd(x))
##New z values,quarterly
zn <- rnorm(h,mean(z),sd(z))
##forecast y values with new data
forecast(eq_u1, list(z = zn, x = xn), method = "static") #alternative method = "dynamic"

#https://github.com/mpiktas/midasr-user-guide/blob/master/midasr-examples.R

