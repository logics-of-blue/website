
# 予測の評価方法：誤差の指標とナイーブな予測 | Logics of Blue
# https://logics-of-blue.com/evaluation-method-of-forecast/
# 2017年07月10日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉


# パッケージのインストール ------------------------------------------------------------

# install.packages("forecast")
library(forecast)



# ホワイトノイズのシミュレーション --------------------------------------------------------

# シミュレーションデータの作成
set.seed(1)
white_noise <- ts(rnorm(n=450, mean=0, sd=10), start=1)

# 訓練データとテストデータに分ける
train_wn <- window(white_noise, end=400)
test_wn <- window(white_noise, start=401)

# 平均値による予測
model_wn <- meanf(train_wn, h=50)
model_wn
mean(train_wn)

# 予測の精度
accuracy(model_wn, test_wn)

# 予測結果のプロット
plot(model_wn)
lines(white_noise)



# ランダムウォークのシミュレーション -------------------------------------------------------

# シミュレーションデータの作成
set.seed(8)
random_walk <- ts(cumsum(rnorm(n=450, mean=0, sd=10)), start=1)

# 訓練データとテストデータに分ける
train_rw <- window(random_walk, end=400)
test_rw <- window(random_walk, start=401)

# 前期と同じ値を予測値として使う
model_rw <- rwf(train_rw,h=50)
model_rw
train_rw[400]

# 予測の精度
accuracy(model_rw, test_rw)



## ランダムウォーク系列には平均値による予測は不適

# 平均値による予測
model_wn_2 <- meanf(train_rw, h=50)

# 2つの予測の比較
par(mfrow=c(2,1), mar=c(2.5,4,2.5,4))

plot(model_rw)
lines(random_walk)

plot(model_wn_2)
lines(random_walk)

par(mfrow=c(1,1))


## 予測の精度比較
# １期間の値を予測値として使ったバージョン
accuracy(model_rw, test_rw)

# 過去平均値を予測値として使ったバージョン
accuracy(model_wn_2, test_rw)


