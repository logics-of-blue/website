
# R言語による時系列予測とクロスバリデーション法による評価 | Logics of Blue
#  https://logics-of-blue.com/time-series-forecast-and-evaluation-with-cv/
# 2018年4月24日：新規作成
# 馬場真哉



# 分析の準備と前処理 ---------------------------------------------------------------

# install.packages("forecast")
# install.packages("ggfortify")
# install.packages("ggplot2")
# install.packages("gridExtra")

library(forecast)
library(ggfortify)
library(ggplot2)
library(gridExtra)


# 対象データ
# Monthly Airline Passenger Numbers 1949-1960
# 対数変換したものを使う
log_air_pass <- log(AirPassengers)
class(log_air_pass)

# 図示
ggtsdisplay(log_air_pass)

# データの確認
print(log_air_pass, digits = 4)

# サンプルサイズ
length(log_air_pass)

# 何年あるか？
length(log_air_pass) / 12


# 訓練データとテストデータに分割
# 最初の10年を訓練、最後の2年をテストデータとする
train <- window(log_air_pass, end=c(1958, 12))
test <- window(log_air_pass, start=c(1959, 1))

# データの長さの確認
length(train)
length(test)


# SARIMAモデルの構築 -------------------------------------------------------------

# モデルを作る
mod_sarima <- auto.arima(train, 
                         ic = "aic",
                         stepwise = F,
                         parallel = TRUE,
                         num.cores = 4)

# 推定結果
mod_sarima

# 残差の評価
checkresiduals(mod_sarima)

# SARIMAによる予測 --------------------------------------------------------------

# 予測期間
h <- length(test)

# 予測
f_sarima <- forecast(mod_sarima, h = h)

# 予測結果の図示
autoplot(f_sarima, main = "SARIMAによる予測")


# テストデータを使った予測の評価 ---------------------------------------------------------

# 予測の評価
accuracy(f_sarima, test)

## 特別な技術を要しない単純な予測手法
# 訓練データの最終時点を予測値とする
f_rwf <- rwf(train, h = h)

# 全体のトレンドを組み込む
f_rwf_drift <- rwf(train, drift = TRUE, h = h)

# 1周期前(今回は12時点前)を予測値とする
f_snaive <- snaive(train, h = h)

# 参考：ナイーブ予測の結果の図示
p1 <- autoplot(f_rwf, main = "訓練データの最終時点を予測値とする")
p2 <- autoplot(f_rwf_drift, main = "トレンド付きのナイーブ予測")
p3 <- autoplot(f_snaive, main = "季節付きのナイーブ予測")
grid.arrange(p1, p2, p3, nrow=3)

# 各々の予測結果のRMSEを比較する
accuracy(f_sarima, test)[, "RMSE"]
accuracy(f_rwf, test)[, "RMSE"]
accuracy(f_rwf_drift, test)[, "RMSE"]
accuracy(f_snaive, test)[, "RMSE"]

## 図示するために、整形をする
# 評価セットの凡例
types <- c("train", "test")
types

# 手作業でデータを整形する。
# このコードをあと3回繰り返す……？
acc_df_sarima <- data.frame(forecast = rep("f_sarima", 2),
                            type = types,
                            RMSE = accuracy(f_sarima, test)[, "RMSE"],
                            row.names = NULL)
acc_df_sarima

# データを簡単に整形できる便利関数を自作する
make_acc_df <- function(forecast_name, types, rmse_vec){
  # グラフにしやすい形式に、予測誤差を整形する関数
  #
  # Args:
  #   forecast_name: 予測の方法の名称
  #   types: 予測誤差のタイプ
  #   rmse_vec: RMSEが格納されたベクトル
  #
  # Returns:
  #   整形された予測誤差のデータフレーム
  
  vec_length <- length(rmse_vec)
  acc_df <- data.frame(forecast = rep(forecast_name, vec_length), 
                       type = types,
                       RMSE = rmse_vec,
                       row.names = NULL)
  return(acc_df)
}

# 動作確認
make_acc_df("f_sarima", types, accuracy(f_sarima, test)[, "RMSE"])

# 4つのRMSEを結合
acc_df <- rbind(
  make_acc_df("f_sarima", types, accuracy(f_sarima, test)[, "RMSE"]), 
  make_acc_df("f_rwf", types, accuracy(f_rwf, test)[, "RMSE"]), 
  make_acc_df("f_rwf_drift", types, accuracy(f_rwf_drift, test)[, "RMSE"]), 
  make_acc_df("f_snaive", types, accuracy(f_snaive, test)[, "RMSE"])
)

# 整形されたデータ。
# 整然データの形式になっているので、扱いやすい
acc_df

# 図示
ggplot(acc_df, aes(x = forecast, y = RMSE, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("SARIMAモデルとナイーブ予測のRMSEの比較")

# クロスバリデーション法による予測の評価 -----------------------------------------------------

# 次数の確認
mod_sarima

# データを入れると、すぐに予測結果が出力される関数を作る
f_sarimax_func <- function(ts_data, h){
  # 訓練データと予測期間を指定すると、予測結果が出力される関数
  #
  # Args:
  #   ts_data:訓練データ
  #   h:予測期間
  #
  # Returns:
  #   forecast関数による予測の結果

  # 選ばれたモデルと同じ次数のARIMAモデルを推定
  mod <- Arima(ts_data, order = c(0,1,1), seasonal = c(0,1,1))
  
  # 予測結果を出力
  return(forecast(mod, h = h))
  
}

# 動作確認
kakunin <- f_sarimax_func(train, h = h)

head(kakunin$mean)
head(f_sarima$mean)

# 1つ目のデータだけを使ってモデル構築～予測誤差計算→エラーが出たらNULL
# 1~2つ目のデータだけを使ってモデル構築～予測誤差計算→エラーが出たらNULL
# 1~3つ目のデータだけを使ってモデル構築～予測誤差計算→エラーが出たらNULL
# というのを繰り返しているらしい
# ■：訓練
# □：テスト
# h=2の時
# ■□□
# ■■□□
# ■■■□□
# ■■■■□□
# ■■■■■□□

# クロスバリデーション法の実行
# h=3で実行
h <- 3
sarima_cv <- tsCV(log_air_pass, f_sarimax_func, h = h)
rwf_cv <- tsCV(log_air_pass, rwf, h = h)
rwf_drift_cv <- tsCV(log_air_pass, rwf, h = h, drift = TRUE)
snaive_cv <- tsCV(log_air_pass, snaive, h = h)

# 結果の確認
sarima_cv

# 予測誤差の図示（訓練データにおける時点も含む）
# どれくらいのデータがあれば、予測結果が落ち着くのかを判断する目安にもなる
autoplot(sarima_cv, main = "SARIMAモデルの予測誤差の推移")

# 予測の評価からは訓練データが含まれる時点を除いたうえで、
# NULLの無い年を使う
sarima_cv_test <- na.omit(window(sarima_cv, start = c(1959, 1)))
rwf_cv_test <- na.omit(window(rwf_cv, start = c(1959, 1)))
rwf_drift_cv_test <- na.omit(window(rwf_drift_cv, start = c(1959, 1)))
snaive_cv_test <- na.omit(window(snaive_cv, start = c(1959, 1)))



# RMSEを計算する関数を作る
calc_rmse <- function(target_resid){
  # 予測残差を入れるとRMSEが出力される関数
  #
  # Args:
  #   target_resid:予測残差(y - y_hat)
  #
  # Returns:
  #   RMSE
  
  return(sqrt(mean(target_resid^2)))
}

# 予測期間別のRMSE
apply(sarima_cv_test, 2, calc_rmse)



## データを整形し、棒グラフを描く

# 凡例
types <- c("h=1", "h=2", "h=3")

# 整形されたデータ
acc_df_cv <- rbind(
  make_acc_df("f_sarima", types, apply(sarima_cv_test, 2, calc_rmse)),
  make_acc_df("rwf", types, apply(rwf_cv_test, 2, calc_rmse)),
  make_acc_df("rwf_drift", types, apply(rwf_drift_cv_test, 2, calc_rmse)),
  make_acc_df("snaive", types, apply(snaive_cv_test, 2, calc_rmse))
)

# 図示
ggplot(acc_df_cv, aes(x = forecast, y = RMSE, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("CVにより評価された予測誤差の比較")


# 予測誤差はどの時点まで変わらない？
sarima_cv_24 <- tsCV(log_air_pass, f_sarimax_func, h = 12)
sarima_cv_24 <- na.omit(window(sarima_cv_24, start = c(1959, 1)))
sarima_cv_24

## データを整形し、棒グラフを描く

# 凡例
types <- paste("h=", formatC(1:12, width=2, flag="0"), sep="")
types

# 整形されたデータ
acc_df_cv_24 <- make_acc_df("sarima_cv_24", types, apply(sarima_cv_24, 2, calc_rmse))

# 図示
ggplot(acc_df_cv_24, aes(x = type, y = RMSE)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("予測対象期間と予測誤差の関係")


# h=3まででやった結果と異なるので、念のため確認
# 1959年だけで見ると、確かにh=3でRMSEが増えている
# 予測期間を延ばすと、評価対象となる時点数が変わることに注意
sarima_cv_test_1959 <- na.omit(window(sarima_cv, start = c(1959, 1), end = c(1959, 12)))
apply(sarima_cv_test_1959, 2, calc_rmse)


# スライド型のクロスバリデーション法の実行 --------------------------------------------------

# 1~window目のデータだけを使ってモデル構築～予測誤差計算→エラーが出たらNULL
# 2~[window+1]目のデータだけを使ってモデル構築～予測誤差計算→エラーが出たらNULL
# 3~[window+2]つ目のデータだけを使ってモデル構築～予測誤差計算→エラーが出たらNULL
# というのを繰り返しているらしい
# ■：訓練
# □：テスト
# ×：使わない
# window=3, h=2の時
# ■■■□□
# ×■■■□□
# ××■■■□□


# h=3, window=訓練データのサンプルサイズで実行
window_cv <- length(train)
h <- 3

# 予測誤差の計算
sarima_window <- tsCV(log_air_pass, f_sarimax_func, h = h, window = window_cv)
sarima_window <- na.omit(window(sarima_window, start = c(1959, 1)))

# RMSEを求める
apply(sarima_window, 2, calc_rmse)

## CVの方法の違いの比較

# 凡例
types <- c("h=1", "h=2", "h=3")

# データの整形
acc_df_cv_pattern <- rbind(
  make_acc_df("sarima_slide", types, apply(sarima_window, 2, calc_rmse)),
  make_acc_df("sarima_fix", types, apply(sarima_cv_test, 2, calc_rmse))
)

# 図示
ggplot(acc_df_cv_pattern, aes(x = forecast, y = RMSE, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("CVの方法の比較")


