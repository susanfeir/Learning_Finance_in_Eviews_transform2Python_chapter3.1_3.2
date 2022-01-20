
library(readxl)
library(zoo)
# library(dplyr)
library(dlm)#状態空間モデル、dynamic linear model使うパケージ
# library(KFAS)
# library(rstan)
# library(stochvol)

#3.3.2 event study example

path = file.path(file.choose()) #choose Chap3.3_data_daily.xlsx
df <- read_excel(path) #toyota asicsの日次データの読み込み
df <- data.frame(df)

#ES:estimation,EV:event
EVDAY <- data.frame(matrix(ncol = 8, nrow = 2),
                    row.names = c("asics","toyota"))　
#イベントウィンドウでの異常収益を計算するために、
# 推定ウィンドウとイベントウィンドウの日付とCAR、SCARを記入するための空白の表を用意
x1 <- c("ESt1", "ESt2","EVENT","EVt1","EVt2","CAR","SCAR","p-val")　#column nameのベクトルを用意
colnames(EVDAY) <- x1　#EVDAYのcolumn nameを用意したものに変更

EVlist <- c(as.Date("2015/11/06"),as.Date("2014/5/8"))#イベント日付を用意
#（2015/11/06はAsicsが業績予想の下方修正、2014/5/8はトヨタ自動車が売上微増を発表）
event_row_a <- which(grepl(EVlist[1], df$Date))　#2015/11/06という日付をdf$Dateの中に探し、該当する行のrow name（index）を取得
event_row_t <- which(grepl(EVlist[2], df$Date))　#2014/5/8という日付をdf$Dateの中に探し、該当する行のrow name（index）を取得

list=list(event_row_a,event_row_t)　#2015/11/06と2014/5/8の該当する行のrow nameリストを作成
for (i in 1:2){
    EVDAY[i,1]=format(as.Date(df$Date[as.numeric(list[i])-21-119]), "%Y/%m/%d")#推定ウィンドウの開始日付を調べ、EVDAYに記入
    EVDAY[i,2] = format(as.Date(df$Date[as.numeric(list[i])-21]), "%Y/%m/%d")#推定ウィンドウの終止日付を調べ、EVDAYに記入
    EVDAY[i,3] = format(as.Date(df$Date[as.numeric(list[i])]), "%Y/%m/%d")#イベント日付を調べ、EVDAYに記入
    EVDAY[i,4] = format(as.Date(df$Date[as.numeric(list[i])-20]), "%Y/%m/%d")#イベントウィンドウの開始日付を調べ、EVDAYに記入
    EVDAY[i,5] = format(as.Date(df$Date[as.numeric(list[i])+20]), "%Y/%m/%d")#イベントウィンドウの終止日付を調べ、EVDAYに記入
}


x2 <- c("rasics", "rtoyota")#調べたい対象の収益率の銘柄のリストを用意
L1=120　#推定ウィンドウの長さ
L2=41　#イベントウィンドウの長さ
for (i in 1:2){
    df_temp1 <- df[(as.numeric(list[i])-21-119):(as.numeric(list[i])-21),]#推定ウィンドウ期間のデータを抜き取る
    y <- ts(df_temp1[x2[i]])　#対象となる銘柄の収益をｙにする
    eq_esWindow <- lm(y~rtopix,data = df_temp1) #推定ウィンドウでのマーケットモデルを回帰推定する
    df_temp2 <- df[(as.numeric(list[i])-20):(as.numeric(list[i])+20),]　#イベントウィンドウ期間のデータを抜き取る
    df_temp2$r_hat <- predict(eq_esWindow,df_temp2)　#eq_esWindowという推定したパラメータを用いてイベントウィンドウの収益率を予測する
    abnormalRe <- df_temp2[x2[i]]-df_temp2$r_hat　##実際の収益率から予測した収益率を引いて、異常リターンを計算
    CAR <- sum(abnormalRe)　#累積異常リターンCAR（cumulative abonormal return）を計算する
    EVDAY[i,6]=CAR #累積異常リターンCARをEVDAYに記入する
    sigma=(sum(eq_esWindow$residuals**2))/(L1-2)　#マーケットモデルの回帰における誤差分散の推定量を計算
    SCAR <- CAR/(sigma^0.5)　#標準化した累積異常リターンSCARを計算する
    EVDAY[i,7]=SCAR　#標準化した累積異常リターンSCARをEVDAYに記入する
    if (SCAR>=0){
      p_value <- pt(SCAR, (L2-2), lower.tail = F) * 2
    }else if ( SCAR<0){
      p_value <- pt(SCAR, (L2-2), lower.tail = T) * 2
    } #SCARが検定統計量よりも小さい確率を見つけ、二倍にして、p値を取得
    EVDAY[i,8]=p_value　##p_valueをEVDAYに記入する
}
EVDAY

#for loopではなく、一つ一つ確認したい時は下記の通り
#トヨタ自動車のCARとSCAR
df_temp1 <- df[(event_row_t-21-119):(event_row_t-21),]#推定ウィンドウ期間のデータを抜き取る
eq_esWindow <- lm(rtoyota~rtopix,data = df_temp1)#推定ウィンドウでのマーケットモデルを回帰推定する
df_temp2 <- df[(event_row_t-20):(event_row_t+20),]#イベントウィンドウ期間のデータを抜き取る
df_temp2$r_hat <- predict(eq_esWindow,df_temp2)#eq_esWindowという推定したパラメータを用いてイベントウィンドウの収益率を予測する
df_temp2$abnormalRe <- df_temp2$rtoyota-df_temp2$r_hat##実際の収益率から予測した収益率を引いて、異常リターンを計算
df_temp2$CAR <- cumsum(df_temp2$abnormalRe)#累積異常リターンCAR（cumulative abonormal return）の推移tsを計算する
plot(df_temp2$CAR,type = "line",main = "CAR2")#トヨタ自動車のCARの推移のグラフ
abline(v=21, col="blue")　#イベント当日
CAR <- sum(df_temp2$abnormalRe)　#イベントウィンドウの最後のCARを取得
EVDAY[2,6]=CAR#累積異常リターンCARをEVDAYに記入する
sigma=(sum(eq_esWindow$residuals**2))/(L1-2)　#マーケットモデルの回帰における誤差分散の推定量を計算
SCAR <- CAR/(sigma^0.5)#標準化した累積異常リターンSCARを計算する
EVDAY[2,7]=SCAR#標準化した累積異常リターンSCARをEVDAYに記入する
p_value <- pt(SCAR, 39, lower.tail = F) * 2　#SCARが検定統計量よりも小さい確率を見つけ、二倍にして、p値を取得
EVDAY[2,8]=p_value#p_valueをEVDAYに記入する

#AsicisのCARとSCAR
df_temp1 <- df[(event_row_a-21-119):(event_row_a-21),]#推定ウィンドウ期間のデータを抜き取る
eq_esWindow <- lm(rasics~rtopix,data = df_temp1)#推定ウィンドウでのマーケットモデルを回帰推定する
df_temp2 <- df[(event_row_a-20):(event_row_a+20),]#イベントウィンドウ期間のデータを抜き取る
df_temp2$r_hat <- predict(eq_esWindow,df_temp2)#eq_esWindowという推定したパラメータを用いてイベントウィンドウの収益率を予測する
df_temp2$abnormalRe <- df_temp2$rasics-df_temp2$r_hat#実際の収益率から予測した収益率を引いて、異常リターンを計算
df_temp2$CAR <- cumsum(df_temp2$abnormalRe)#累積異常リターンCAR（cumulative abonormal return）の推移tsを計算する
plot(df_temp2$CAR,type = "line",main = "CAR1")##AsicsのCARの推移のグラフ
abline(v=21, col="blue")#イベント当日
CAR <- sum(df_temp2$abnormalRe)#イベントウィンドウの最後のCARを取得
EVDAY[1,6]=CAR#累積異常リターンCARをEVDAYに記入する
sigma=(sum(eq_esWindow$residuals**2))/(L1-2)　#マーケットモデルの回帰における誤差分散の推定量を計算
SCAR <- CAR/(sigma^0.5)#標準化した累積異常リターンSCARを計算する
EVDAY[1,7]=SCAR#標準化した累積異常リターンSCARをEVDAYに記入する
p_value <- pt(SCAR, 39, lower.tail = T) * 2　#SCARが検定統計量よりも小さい確率を見つけ、二倍にして、p値を取得
EVDAY[1,8]=p_value#p_valueをEVDAYに記入する

#3.3.3 state space model(use asics data)

df_temp1 <- df[(event_row_a-21-119):(event_row_a-21),]#Asicsの推定ウィンドウ期間のデータを抜き取る
df_temp2 <- df[(event_row_a-20):(event_row_a+20),]#Asicsのイベントウィンドウ期間のデータを抜き取る
Rt <- df_temp1$rasics #Asicsのマーケットモデルの被説明変数を用意
Rmt <- df_temp1$rtopix #Asicsのマーケットモデルの説明変数を用意
#method 1
#https://logics-of-blue.com/dlm%E3%81%AB%E3%82%88%E3%82%8B%E6%99%82%E5%A4%89%E4%BF%82%E6%95%B0%E3%83%A2%E3%83%87%E3%83%AB/
#https://ameblo.jp/pistacian/entry-12520247836.html

# V Variance of the observations (scalar)
# W Variance of the state variables’ error terms (matrix)
# m0 Initial values of the state variables (vector)
buildModReg <- function(v) {
  dV <- exp(v[1])# Variances for observation errors,epsilon(variance of epsilon is C3,initial_value=se(ols.residual)^2)
  dW <- c(0,exp(v[2])) # Variances of beta0(C1), alpha(SV1), Note zero variance for beta0
  # m0 <- v[3:4] # Initial levels for beta0(C1), alpha(SV1,initial_value=1)
  dlmModReg(Rmt, dV = dV, dW = dW)
}　#Create a DLM(dynamic linear model) representation of a regression model

parm <- c(log(sigma),log(1)) #パラメータの初期値を設定する
mle <- dlmMLE(Rt, parm = parm, build = buildModReg, method="BFGS") #MLE(maximum likelihood estimation)でパラメタ推定#https://www.imsbio.co.jp/RGM/R_rdfile?f=stats/man/optim.Rd&d=R_rel
if (mle$convergence != 0) stop(mle$message)　#収束しないと停止する
sqrt(exp(mle$par)/120)#s.e.
#推定されたパラメタにexpをとると、分散の値になります。
exp(mle$par)　#最適化したパラメータをチェック
model <- buildModReg(mle$par)　#推定された分散を使って、モデルを組みなおす
modelF <- dlmFilter(Rt,model)#フィルタリング
# dropFirst(modelF$m)#カルマンフィルター
smooth <- dlmSmooth(modelF)　#平滑化する
s <- dropFirst(smooth$s) ##スムージングの結果beta0,alpha
plot(df_temp1$Date,s[,2],type='line',main = "SV1F",ylab = 'alpha',xlab = 'Date-Month')#平滑化したalphaのグラフを作成
s

filt2 <- dlmFilter(df_temp2$rtopix, model)#イベントウィンドウのデータを用いて、フィルタリング
s2 <- dlmSmooth(filt2)#平滑化する
s2 <- s2$s　#平滑化したvalue
forc2 <- dropFirst(s2)[,1]+dropFirst(s2)[,2]*df_temp2$rtopix #平滑化したvalueを用いて予測する
df_temp2$ar1 <- as.numeric(df_temp2$rasics-forc2)　#観測値から予測値を引いて、異常リターンを計算
plot(df_temp2$ar1,type='line',main = "AR1",ylab = 'AR',xlab = 'Date')#異常リターンのグラフを作成
abline(v=21,col="red")　#イベントの日
df_temp2$car1 <- cumsum(df_temp2$ar1)　#累積異常リターンCARを計算
plot(df_temp2$car1,type='line',main = "CAR1",ylab = 'CAR',xlab = 'Date')#累積異常リターンCARのグラフを作成
abline(v=21,col="red") #イベントの日


########################################################################
# # #method 2
# # 
# stan_code <- "
# data {
#   int T;         // データ取得期間の長さ
#   vector[T] Rt;   // 観測値
#   vector[T] Rmt;   // explanatory値
# }
# 
# parameters {
# 
#   real<lower=0> s1;  // 過程誤差の標準偏差
#   real<lower=0> s2;  // 観測誤差の標準偏差 sigma
#   real alpha[T];
#   real beta0;
#   real beta1;
#   real alpha_zero;
# }
# 
# model {
#   beta0 ~ normal(0, 1);
#   beta1 ~ normal(0, 1);
#   alpha[1]~ normal(0, 1);
#   for(t in 2:T) {
#     alpha[t] ~ normal(beta1*alpha[t-1], s1);
#   }
# 
#   for(t in 1:T) {
#     Rt[t] ~ normal(beta0+alpha[t]*Rmt[t], s2);
#   }
# }
# "

# library(rstan)
# data_stan = list(T=120,Rt=Rt,Rmt=Rmt)
# # mod_stan = stan_model(model_code = stan_code)
# initf1 <- function() {
#   list(s1=1,s2 = 0.000333, beta1 = 1)
# }
# 
# mod_ll <- stan(file = "D:\\Chrome Download\\07973_1\\Chap3_marketRiskAnalysis\\stan_code.stan",
#                data = data_stan,iter=2000,chains = 4,init = initf1)#
# mod_ll
# print(mod_ll, pars = c("s1", "s2","beta0","beta1"))
# SV1 <- get_posterior_mean(mod_ll, par = 'alpha')
# SV1[, 'mean-all chains']
# plot(df_temp1$Date,SV1[, 'mean-all chains'])
