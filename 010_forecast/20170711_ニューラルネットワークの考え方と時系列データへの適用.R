
# ニューラルネットワークの考え方と時系列データへの適用 | Logics of Blue
# http://logics-of-blue.com/time-series-analysis-by-nnet/
# 2017年7月12日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉


# nnetの使い方 ----------------------------------------------------------------

# install.packages("nnet")
library(nnet)

# 自動車の速度と、停止距離の関係
cars

# 普通の線形回帰
lm_model <- lm(dist ~ speed, data=cars)

# ニューラルネットワーク
nnet_model <- nnet(
  dist ~ speed, data=cars, 
  size=40, linout=TRUE, maxit=1000
)

# 中間層をなくしたニューラルネットワーク（実質線形回帰と同じ）
lm_modoki <- nnet(
  dist ~ speed, data=cars, 
  size=0,skip=TRUE,  linout=TRUE
)

# 係数が同じになっている
summary(lm_modoki)
lm_model

# 図示
plot(dist ~ speed, cars)
lines(lm_model$fit ~ cars$speed)
lines(nnet_model$fitted.values ~ cars$speed, col=2)
lines(lm_modoki$fitted.values ~ cars$speed, col=4, lty=2, lwd=2)
legend(
  "topleft",
  legend=c("線形回帰", "ニューラルネット", "中間層がないニューラルネット"),
  col=c(1,2,4),
  lty=c(1,1,2)
)



# 飛行機乗客数のモデル化 -------------------------------------------------------------

# パッケージの読み込み
# install.packages("forecast")
library(forecast)

# 飛行機乗客数のデータ
AirPassengers
frequency(AirPassengers)

# 訓練データとテストデータに分ける
train_air <- window(AirPassengers, end = c(1957, 12))
test_air <- window(AirPassengers, start = c(1958, 1))

train_air
test_air

# 時系列データへのニューラルネットワーク
set.seed(0)
nn_air <- nnetar(train_air, maxit=1000)

# 推定された結果
nn_air

# 中身を確認
names(nn_air)
summary(nn_air$model)

# repeatsを変更すると、モデルの数も変わる(デフォルトは20)
summary(nnetar(train_air, maxit=1000, repeats=2)$model)

# 推定された重みを確認する
summary(nn_air$model[[1]])


# 比較のためにSARIMAモデルも使う
sarima_air <- auto.arima(train_air, ic = "aic")

# SARIMAの推定結果
sarima_air

# 3年後までを予測する
# nnet
f_nn_air <- forecast(nn_air, h = 36)$mean

# sarima
f_sarima_air <- forecast(sarima_air, h=36)$mean

# 予測精度の比較
accuracy(f_nn_air, test_air)
accuracy(f_sarima_air, test_air)

# 予測結果のグラフ
ts.plot(
  AirPassengers, 
  f_nn_air,
  f_sarima_air,
  ylim=c(90, 600),
  col=c(1,2,4),
  lwd = c(1,2,2),
  main="ARIMAとNNの比較"
)
legend(
  "topleft",
  legend=c("元データ", "ニューラルネット","SARIMA"),
  col=c(1,2,4),
  lwd=c(1,2,2)
)



# ヤマネコ個体数データのモデル化 ---------------------------------------------------------

# ヤマネコ個体数データ
lynx

# 訓練データとテストデータに分ける
train_lynx <- log(window(lynx, end = c(1920)))
test_lynx <- window(lynx, start = c(1921))

train_lynx
test_lynx

# 時系列データへのニューラルネットワーク
nn_lynx <- nnetar(train_lynx, decay=0.5, maxit=1000)

# 比較のためにARIMAモデルも使う
arima_lynx <- auto.arima(train_lynx, ic = "aic")

# 推定結果
nn_lynx
arima_lynx

# 次数を合わせるため、11期前までを使ったARIMAも作る
arima_lynx_11 <- Arima(train_lynx, order=c(11,0,10))
arima_lynx_11

# 予測する
# nnet
f_nn_lynx <- forecast(nn_lynx, h = 14)$mean

# arima
f_arima_lynx <- forecast(arima_lynx, h=14)$mean
f_arima_lynx_11 <- forecast(arima_lynx_11, h=14)$mean


# 予測精度の比較
accuracy(exp(f_nn_lynx), test_lynx)
accuracy(exp(f_arima_lynx), test_lynx)
accuracy(exp(f_arima_lynx_11), test_lynx)

# 予測結果のグラフ
ts.plot(
  lynx, 
  exp(f_nn_lynx),
  exp(f_arima_lynx),
  exp(f_arima_lynx_11),
  col=c(1,2,3,4),
  lwd = c(1,2,2,2),
  main="ARIMAとNNの比較"
)
legend(
  "topright",
  legend=c("元データ", "NNAR(11,6)","ARIMA(3,0,2)","ARIMA(11,0,10)"),
  col=c(1,2,3,4),
  lwd=c(1,2,2,2)
)



# 発展的な話題 ------------------------------------------------------------------


## リミットサイクルを描く
# 長期間における周期変動を調べる
f_nn_lynx_long <- forecast(nn_lynx, h = 40)$mean
f_nn_lynx_long
forecast_lag <- embed(f_nn_lynx_long, 2)
plot(
  forecast_lag[,1] ~ forecast_lag[,2], 
  type="l", main="リミットサイクル",
  xlab="t-1", ylab="t"
)

# 参考 embedの動き
embed(1:5, 2)

## ニューラルネットワークにおける予測区間
# ブートストラップ法を使って、予測区間を出す
f_nn_lynx2 <- forecast(
  nn_lynx, h = 14, 
  bootstrap = T, level = c(80,95),  PI=T
)
f_nn_lynx2

# 予測区間のあるグラフ
plot(f_nn_lynx2)
lines(log(test_lynx), col=2)

# ggplot2を使ったグラフ
# install.packages("ggplot2")
library(ggplot2)
autoplot(f_nn_lynx2)


