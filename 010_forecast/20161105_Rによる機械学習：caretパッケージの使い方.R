
# Rによる機械学習：caretパッケージの使い方 | Logics of Blue
# https://logics-of-blue.com/rによる機械学習：caretパッケージの使い方/
# 2016年11月05日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉



# シミュレーションでデータを作る:回帰 ------------------------------------------------------

# データの作成
set.seed(0)
N <- 1500
x1 <- runif(N, min = -5, max = 5)
x2 <- runif(N, min = -10, max = 10)
x3 <- runif(N, min = 0, max = 10)

y <- (sin(x1 *  pi/2)*4 + x1*0.5) * x2 + x3*2 + (x3^2)*0.4 + rnorm(N, mean = 0, sd = 1)

# 図示
par(mfrow=c(3,1))
plot(y ~ x1, main="x1")
plot(y ~ x2, main="x2")
plot(y ~ x3, main="x3")
par(mfrow=c(1,1))


# 3次元プロット
# install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(x1, x2, y)

# data.frameにまとめる。
dataRegression <- data.frame(
  y = y,
  x1 = x1,
  x2 = x2,
  x3 = x3
)

# テスト用と学習用にデータを分ける
dataRegressionTrain <- dataRegression[1:1000,]
dataRegressionTest <- dataRegression[1001:1500,]



# 重回帰分析による予測 --------------------------------------------------------------

# 重回帰モデルの構築
modelLm <- lm(
  y ~ (.)^2, 
  data = dataRegressionTrain
)

# AICによる変数選択
modelLm <- step(modelLm)
modelLm


# caretパッケージによる予測 ---------------------------------------------------------


# 必要であれば、caretパッケージをインストール

# install.packages("caret")
library(caret)

# 並列化演算を行う
# install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(detectCores()) # detectCores()とすることで、コア数を取得できる
registerDoParallel(cl)


# ニューラルネットワークによる予測
set.seed(0)
modelNnet <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain,
  method = "nnet", 
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv"),
  tuneGrid = expand.grid(size=c(1:10), decay=seq(0.1, 1, 0.1)),
  linout = TRUE
)

# 推定結果
modelNnet


# ランダムフォレストによる予測
# randomForestパッケージを使う。
set.seed(0)
modelRF <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain, 
  method = "rf", 
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)


# rangerパッケージを使う。
set.seed(0)
modelRanger <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain, 
  method = "ranger", 
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)


# Rboristパッケージを使う。
set.seed(0)
modelRborist <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain, 
  method = "Rborist", 
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)

# 推定結果
modelRF
modelRanger
modelRborist




# 予測の評価 -------------------------------------------------------------------

# 結果の一覧
modelLm
modelNnet
modelRF
modelRanger
modelRborist

# テストデータで予測する
predLm <- predict(modelLm, dataRegressionTest)
predNnet <- predict(modelNnet, dataRegressionTest)
predRF <- predict(modelRF, dataRegressionTest)
predRanger <- predict(modelRanger, dataRegressionTest)
predRborist <- predict(modelRborist, dataRegressionTest)

# RMSE
sqrt( sum((dataRegressionTest$y - predLm)^2) / 500 )
sqrt( sum((dataRegressionTest$y - predNnet)^2) / 500 )
sqrt( sum((dataRegressionTest$y - predRF)^2) / 500 )
sqrt( sum((dataRegressionTest$y - predRanger)^2) / 500 )
sqrt( sum((dataRegressionTest$y - predRborist)^2) / 500 )




# 機械学習による判別 ---------------------------------------------------------------


# アヤメのデータを使う。
# アヤメの種類を判別する
head(iris)

# 訓練データとテストデータに分割
indexIris <- which(1:nrow(iris)%%3 == 0)
indexIris
irisTrain <- iris[-indexIris,]
irisTest <- iris[indexIris,]


# ニューラルネットワーク
set.seed(0)
irisNnet <- train(
  Species ~ (.)^2, 
  data = irisTrain, 
  method = "nnet", 
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv"),
  linout = F
)

# ランダムフォレスト（RandomForestパッケージ）
set.seed(0)
irisRF <- train(
  Species ~ (.)^2, 
  data = irisTrain, 
  method = "rf", 
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)

# 結果
irisNnet
irisRF


# 予測
# ニューラルネットワーク
predIrisNnet <- predict(irisNnet, irisTest)
# ランダムフォレスト（RandomForestパッケージ）
predIrisRF <- predict(irisRF, irisTest)

# 評価
# ニューラルネットワーク
confusionMatrix(data = predIrisNnet, irisTest$Species)
# ランダムフォレスト（RandomForestパッケージ）
confusionMatrix(data = predIrisRF, irisTest$Species)


# もっと複雑な手法を試してみる ----------------------------------------------------------


# 勾配ブースティングによる予測 xgboost:線形予測の結合
set.seed(0)
modelXgboostLinear <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain,
  method = "xgbLinear", 
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv"),
  tuneLength = 4
)



# 勾配ブースティングによる予測 xgboost:Treeモデルの結合
set.seed(0)
modelXgboostTree <- train(
  y ~ (.)^2, 
  data = dataRegressionTrain,
  method = "xgbTree", 
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv"),
  tuneLength = 4
)



# 推定結果
modelXgboostLinear
modelXgboostTree

# 予測
predxgBoostLinear <- predict(modelXgboostLinear, dataRegressionTest)
predxgBoostTree <- predict(modelXgboostTree, dataRegressionTest)


# RMSEを、最高精度だったランダムフォレストと比較
sqrt( sum((dataRegressionTest$y - predRF)^2) / 500 )
sqrt( sum((dataRegressionTest$y - predxgBoostLinear)^2) / 500 )
sqrt( sum((dataRegressionTest$y - predxgBoostTree)^2) / 500 )



