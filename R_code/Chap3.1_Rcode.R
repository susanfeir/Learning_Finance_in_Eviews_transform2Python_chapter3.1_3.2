
library(readxl)
path = file.path(file.choose())
# Chap3.1_3.4_data_daily
df <- read_excel(path,sheet = "Sheet2")
df <- data.frame(df)
# set index
rownames(df) <- df$Date

#インポートした国債利回り（日次）のデータはパーセント単位（半年複利の利回り）なので、
#対数階差で計算する収益率は年次なので、次の式で半年複利の利回りm10を年利に変更する。
df$m10y <- ((1+df$m10*0.01/2)^2-1)
#m10*0.01 小数に戻す
#df$m10*0.01/2 半年を年に変更
#1+df$m10*0.01/2　元本に
# (1+df$m10*0.01/2)^2　複利計算は二乗
# (1+df$m10*0.01/2)^2-1　金利の部分だけを取る

#超過リターンを計算する
r1 <- (diff(log(df$asics)))-df$m10y[-1]
r1 <- c(NA,r1)
df <- cbind(df,r1)
r2 <- (diff(log(df$gree)))-df$m10y[-1]
r2 <- c(NA,r2)
df <- cbind(df,r2)
r3 <- (diff(log(df$mufj)))-df$m10y[-1]
r3 <- c(NA,r3)
df <- cbind(df,r3)
r4 <- (diff(log(df$tgas)))-df$m10y[-1]
r4 <- c(NA,r4)
df <- cbind(df,r4)
r5 <- (diff(log(df$toyota)))-df$m10y[-1]
r5 <- c(NA,r5)
df <- cbind(df,r5)
rtpx <- (diff(log(df$topix)))-df$m10y[-1]
rtpx <- c(NA,rtpx)
df <- cbind(df,rtpx)

#空白の結果の表を作り、後で推定したOLSの結果を書き込む準備をする
result_df <- data.frame(matrix(ncol = 2, nrow = 10),row.names = c("ASICS","ASICS-p","GREE","GREE-p","MUFJ","MUFJ-p","TGAS","TGAS-p","TOYOTA","TOYOTA-p"))
x1 <- c("ALPHA", "BETA")
colnames(result_df) <- x1


#それぞれの超過リターンのシングルインデックスモデルの推定
for (i in 1:5){
  y<-  get((paste0("r",i)))
  eq <- lm(y~df$rtpx)
  result_df[i*2-1,1]=summary(eq)$coefficients[1]
  result_df[i*2-1,2]=summary(eq)$coefficients[2]
  result_df[i*2,1]=summary(eq)$coefficients[,3][1]
  result_df[i*2,2]=summary(eq)$coefficients[,3][2]
}

