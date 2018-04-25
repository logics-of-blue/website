
# カオス時系列の基礎とニューラルネットワーク | Logics of Blue
# https://logics-of-blue.com/カオス時系列の基礎とニューラルネットワーク/
# 2016年10月23日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉



# ロジスティック写像 ---------------------------------------------------------------

# 必要であればパッケージをインストール
# install.packages("nonlinearTseries")

library(nonlinearTseries)

# nonlinearTseriesの関数を使ってシミュレーションしてみる
logMap <- logisticMap(
  r = 4, 
  n.sample = 100, 
  start = 0.4, 
  n.transient = 0, 
  do.plot = TRUE
)

# ロジスティック曲線の例
K <- 1
b <- 3
c <- 1

x <- seq(-5, 10, 0.1)
y <- K / (1 + b * exp(-c * x))

plot(y ~ x, type = "l", main="ロジスティック曲線")

# embedの使い方
1:5
embed(1:5, 2)

# データをずらす
lagData <- embed(y, 2)
lagData[1:5,]

# 差分をとる。これが「増加値」になる
diffData <- lagData[,1] - lagData[,2]

# yの増加値を縦軸に、yの値そのものを横軸に置いたグラフ
plot(
  diffData ~ lagData[,1], 
  ylab = "yの増加値", 
  xlab = "yの値", 
  main = "yの増加値の変化"
)


# ロジスティック写像のデータをずらしてプロットしてみる
lagLogMap <- embed(logMap, 2)

plot(
  lagLogMap[,1] ~ lagLogMap[,2], 
  ylab = "今期の値", 
  xlab = "前期の値", 
  main = "今期の値と前期の値の比較"
)

# 2期目の値
4 * 0.4 * (1 - 0.4)
logMap


# 定義通りに計算してみる
x0 <- 0.4            # 初期値
x <- numeric(100)    # カオス時系列を入れる入れ物
x[1] <- x0
r <- 4               # パラメタ
for(i in 2:100){
  x[i] <- r * x[i-1] * (1 - x[i-1])
}

# 結果は同じ
x[-1]
logMap


# ロジスティック写像の特徴 ------------------------------------------------------------


# 初期値をわずかに変えてみる
logMap2 <- logisticMap(
  n.sample = 100, 
  start = 0.400000001, 
  n.transient = 0, 
  do.plot = F
)

# 初期値0.4の時のロジスティック写像と比較
ts.plot(
  ts(logMap), 
  ts(logMap2), 
  col = c(1,2), 
  lty = c(1,2), 
  lwd = c(2,1),
  main = "初期値を変えたときの比較"
)


# パラメタを変えてみる
logMap3 <- logisticMap(
  r = 3.5, 
  n.sample = 100, 
  start = 0.4, 
  n.transient = 0, 
  do.plot = T
)


# リアプノフ指数 -----------------------------------------------------------------


# ロジスティック写像の微分
logMapDifferential <- function(r, x){
  return(-2 * r * x + r)
}


# リアプノフ指数が正なので、カオス
sum(log(abs(logMapDifferential(4, logMap))))/ 99


# これはカオスではない（周期的変動）なので、リアプノフ指数も負になる
sum(log(abs(logMapDifferential(3.5, logMap3))))/ 99



# サロゲートテスト ----------------------------------------------------------------


# リアプノフ指数が正だったカオス時系列は有意
surrogateTest(
  time.series = logMap,
  significance = 0.05,
  K = 1, 
  one.sided = FALSE,
  FUN = timeAsymmetry
)

# 正規乱数を入れてみても、棄却されない
set.seed(1)
surrogateTest(
  time.series = rnorm(100),
  significance = 0.05,
  K = 1, 
  one.sided = FALSE,
  FUN = timeAsymmetry
)



# ARIMAモデルによる予測 -----------------------------------------------------------


# 必要であればパッケージをインストール
# install.packages("forecast")

library(forecast)

logMapArima <- auto.arima(
  logMap,
  ic = "aic",
  trace = T,
  stepwise = F,
  approximation = F
)

# arima(0,0,0)すなわち、ただのホワイトノイズだとみなされてしまった。
logMapArima

# 101期目以降を予測しても、もちろん当たらない
logMapNext <- logisticMap(
  r = 4, 
  n.sample = 120, 
  start = 0.4, 
  n.transient = 0, 
  do.plot = FALSE
)

plot(forecast(logMapArima, h=20))
lines(logMapNext)

# 予測精度の計算
f <- forecast(logMapArima, h=20)$mean
sqrt(sum((f - logMapNext[100:119])^2)/20) # RMSE
sum(abs(f - logMapNext[100:119]))/20      # MAE
accuracy(forecast(logMapArima, h=20),logMapNext[100:119])



# ニューラルネットワークによる予測 --------------------------------------------------------


set.seed(1)
logMapNnet <- nnetar(
  y = logMap,
  p = 1,
  size = 4
)

plot(forecast(logMapNnet, h=20))
lines(logMapNext)

# ニューラルネットワークの予測精度
accuracy(forecast(logMapNnet, h=20),logMapNext[100:119])


# 5期先までのみを予測する
accuracy(forecast(logMapNnet, h=5),logMapNext[100:104])



