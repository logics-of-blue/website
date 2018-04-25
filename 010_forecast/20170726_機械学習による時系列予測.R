
# 機械学習による時系列予測 | Logics of Blue
# http://logics-of-blue.com/time-series-forecast-by-machine-learning/
# 2017年07月26日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉



# 解析の準備 -------------------------------------------------------------------


# 原系列
AirPassengers

## 対数差分系列への適用
log_diff_passenger <- diff(log(AirPassengers))
log_diff_passenger

# 図示
par(mfrow=c(2,1))
plot(AirPassengers, main="原系列")
plot(log_diff_passenger, main="対数差分系列")
par(mfrow=c(1,1))


# 元に戻す
exp(cumsum(log_diff_passenger) + log(AirPassengers[1]))

# 参考
as.numeric(AirPassengers)[-1]


## ラグをとって、過去のデータを説明変数にする
# embed関数による整形
lag_num <- 4
exp_val_sample <- as.data.frame(embed(log_diff_passenger, lag_num))

# 列名の変更
colnames(exp_val_sample)[1] <- "Y"
for(i in 2:lag_num){
  colnames(exp_val_sample)[i] <- paste("Lag", i-1, sep="")
}

# 整形されたデータ
head(exp_val_sample)


## caretによる学習モデル作成の準備
# install.packages("kernlab")
# install.packages("caret")
# install.packages("e1071")
library(caret)
library(kernlab)

# 並列化演算を行う
# install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)



# Rによる機械学習 ----------------------------------------------------------------


# モデルの推定
set.seed(0)
tuned_svm_sample <- train(
  Y ~ ., 
  data=exp_val_sample,
  method = "svmRadial", 
  tuneGrid = expand.grid(C=c(1:5), sigma=2^c(-1:1)),
  preProcess = c('center', 'scale')
)

# チューニングされたモデル
tuned_svm_sample

# 最小のRMSE（予測誤差）
tuned_svm_sample$results$RMSE
min(tuned_svm_sample$results$RMSE)

# チューニングされたハイパーパラメタ
tuned_svm_sample$bestTune

# SVMのモデルだけを取り出す
tuned_svm_sample$finalModel



# 機械学習による時系列予測 ------------------------------------------------------------


## 最適な次数を選ぶ
# 予測精度などを格納する入れ物
sim_result <- data.frame(
  order=numeric(), 
  error=numeric(),
  C=numeric(),
  sigma=numeric()
)

# 予測モデルの一覧を格納する入れ物
tuned_models <- list()

# 最大ラグ数
lag_max <- 12

# 訓練データの作成
# 最後のデータだけテスト用に残しておく
exp_val <- as.data.frame(embed(log_diff_passenger[-length(log_diff_passenger)], lag_max + 1))
colnames(exp_val)[1] <- "Y"
for(i in 2:(lag_max+1)){
  colnames(exp_val)[i] <- paste("Lag", i-1, sep="")
}

# 訓練データ表示
head(exp_val)

# ループさせて、最も予測精度が高くなるラグ数を調べる
set.seed(0)
for(i in 1:lag_max) {

  # 必要なラグまで、データを切り落とす
  exp_val_tmp <- exp_val[,c(1:(i+1))]
  
  # 予測モデルの作成
  tuned_svm <- train(
    Y ~ ., 
    data=exp_val_tmp,
    method = "svmRadial", 
    tuneGrid = expand.grid(C=c(1:3), sigma=2^c(-1:1)),
    preProcess = c('center', 'scale')
  )
  
  # 予測モデルを保存する
  tuned_models <- c(tuned_models, list(tuned_svm))
  
  # 予測精度などを保存する
  sim_result[i, "order"] <- i
  sim_result[i, "error"] <- min(tuned_svm$results$RMSE)
  sim_result[i, "sigma"] <- tuned_svm$bestTune["sigma"]
  sim_result[i, "C"]     <- tuned_svm$bestTune["C"]
  
}


# ラグと予測精度の関係
plot(
  sim_result$error ~ sim_result$order,
  main="次数と予測誤差の関係",
  xlab="order",
  ylab="error"
)

# 最も予測精度が高かったモデルの評価結果
subset(sim_result, sim_result$error==min(sim_result$error))

# 次数だけを取り出す
best_order <- subset(sim_result, sim_result$error==min(sim_result$error))$order
best_order

## 当てはまりの精度の確認
# 最も予測精度が高かったモデルを取り出す
best_model <- tuned_models[[best_order]]
best_model

# 予測値と実測値のプロット
plot(log_diff_passenger[-c(1:lag_max, length(log_diff_passenger))], type="l")
lines(predict(best_model), col=2, lwd=2, lty=2)
legend(
  legend=c("実測値", "予測値"),
  "topright",
  col=c(1,2),
  lwd=c(1,2),
  lty=c(1,2)
)



## 1期先の将来予測

# 説明変数を作る
# 直近のデータだけを抽出
last_data <- exp_val[nrow(exp_val), 1:best_order]

# 列名を、学習データに合わせる
for(i in 1:best_order){
  colnames(last_data)[i] <- paste("Lag", i, sep="")
}

# 説明変数を表示
last_data

# 1期先の予測
predict(best_model, last_data)

# 正解データと比較
log_diff_passenger[length(log_diff_passenger)]





