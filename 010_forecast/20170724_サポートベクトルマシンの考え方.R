
# サポートベクトルマシンの考え方 | Logics of Blue
# http://logics-of-blue.com/svm-concept/
# 2017年7月24日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 2018年06月20日：サポートベクトル回帰のコードを修正
# 馬場真哉



# 線形サポートベクトル分類 ------------------------------------------------------------


# install.packages("kernlab")
library(kernlab)

# サンプルデータ
bird <- data.frame(
    wing = c(12, 10, 13, 10, 13, 12),
    body = c(15, 20, 23, 30, 36, 39),
    type = c("A","A", "A", "B", "B", "B")
)

# 図示
plot(
  wing ~ body, 
  data=bird, 
  type="n",
  main="鳥の羽と体の大きさ"
)
text(
  wing ~ body, 
  data=bird, 
  rownames(bird),
  col=c(1,2)[bird$type],
  cex=2
)


# 線形のSV分類
svm_bird <- ksvm(
  type ~ wing + body, 
  data=bird,
  type="C-svc",
  kernel="vanilladot"
)

# 結果
svm_bird

# サポートベクトル
SVindex(svm_bird)

# 分類境界の図示
plot(svm_bird, data=bird)




# 非線形サポートベクトル分類 -----------------------------------------------------------

bird_2 <- data.frame(
  wing = c(12, 12, 10, 13, 10, 13, 12, 12, 12, 12, 11),
  body = c(10, 15, 20, 22, 34, 36, 39, 37, 25, 29, 27),
  type = c("A", "A", "A", "A", "A", "A","A", "A", "B", "B", "B")
)

# 図示
plot(
  wing ~ body, 
  data=bird_2, 
  type="n",
  main="鳥の羽と体の大きさ(非線形)"
)
text(
  wing ~ body, 
  data=bird_2, 
  rownames(bird_2),
  col=c(1,2)[bird_2$type],
  cex=2
)


# 参考
# 線形のSV分類。これだとうまくいかない
svm_bird_dame <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="vanilladot",
  C=25
)

# 分類境界の図示
plot(svm_bird_dame, data=bird_2)

# 分類結果
fitted(svm_bird_dame)



## 非線形にも対応するイメージ
# 非線形変換の考え方
# （あくまで参考。SVMの実際の計算とは異なります）
new_val <- (bird_2$wing + bird_2$body -39)^2
# 2乗した結果を新たに変数として使えば、分類できそう
plot(
  new_val ~ bird_2$body, 
  col=c(1,2)[bird_2$type], 
  pch=16,
  main="非線形変換をした例"
)


# 非線形のSV分類
# polydot（多項式カーネル）
svm_bird_2 <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="polydot",
  kpar = list(degree=2),
  C=25
)

# 結果
svm_bird_2


# 分類境界の図示
plot(svm_bird_2, data=bird_2)



# 非線形のSV分類
# rbfdot（ガウシアンカーネル）
svm_bird_3 <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="rbfdot",
  kpar = list(sigma=1),
  C=25
)

# 結果
svm_bird_3

# サポートベクトル
SVindex(svm_bird_3)

# 分類境界の図示
plot(svm_bird_3, data=bird_2)


# 非線形のSV分類
# rbfdot（ガウシアンカーネル）
# sigmaを増やした
svm_bird_4 <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="rbfdot",
  kpar = list(sigma=20),
  C=25
)

# 分類境界の図示
plot(svm_bird_4, data=bird_2)




# 非線形のSV分類
# rbfdot（ガウシアンカーネル）
# Cを減らした
svm_bird_5 <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="rbfdot",
  kpar = list(sigma=1),
  C=0.1
)

# 分類境界の図示
plot(svm_bird_5, data=bird_2)


# 結果
fitted(svm_bird_5)


# サポートベクトル回帰 --------------------------------------------------------------

# 予測モデルの作成
svm_air <- ksvm(
  Ozone ~ Temp,
  data=airquality,
  epsilon=0.25,
  kernel="rbfdot",
  kpar=list(sigma=1),
  C=2
)

# 予測
new <- data.frame(
  Temp=seq(min(airquality$Temp), max(airquality$Temp), 0.1)
)

svm_air_pred <- predict(svm_air, new)

# 予測結果の図示
plot(
  airquality$Ozone ~ airquality$Temp,
  xlab="Temp", 
  ylab="Ozone"
)
lines(svm_air_pred ~ as.matrix(new), col=2, lwd=2)




# ハイパーパラメタのチューニング ---------------------------------------------------------


# install.packages("caret")
# install.packages("e1071")
library(caret)
library(e1071)

# 並列化演算を行う
# install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# 鳥のデータ使ってチューニング
set.seed(0)
tuned_svm <- train(
  type ~ wing + body, 
  data=bird_2,
  method = "svmRadial", 
  tuneGrid = expand.grid(C=c(1:30), sigma=seq(0.1, 2, 0.1)),
  preProcess = c('center', 'scale')
)

# チューニングの結果
tuned_svm



# 最も「よい」パラメタを使って境界線を図示
best_svm <- ksvm(
  type ~ wing + body, 
  data=bird_2,
  type="C-svc",
  kernel="rbfdot",
  kpar = list(sigma=0.8),
  C=3
)

plot(best_svm, data=bird_2)



