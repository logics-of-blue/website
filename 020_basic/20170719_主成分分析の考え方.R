
# 主成分分析の考え方 | Logics of Blue
# http://logics-of-blue.com/principal-components-analysis/ 
# 2017年7月20日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉



# パッケージのインストール ------------------------------------------------------------

# install.packages("ggplot2")
# install.packages("GGally")
# install.packages("devtools")
# devtools::install_github("vqv/ggbiplot")



# 主成分分析の概要 ----------------------------------------------------------------


# サンプルデータ
sample_data <- data.frame(
  X = c(2,4, 6, 5,7, 8,10),
  Y = c(6,8,10,11,9,12,14)
)

plot(Y ~ X, data=sample_data, main="元のデータ")

# 主成分分析の実行
pcr_model_sample <- prcomp(sample_data, scale=T)

# 結果
summary(pcr_model_sample)

# 主成分得点
pcr_model_sample$x

# 図示
par(mfrow=c(1,2))
plot(Y ~ X, data=sample_data, main="元のデータ")
biplot(pcr_model_sample, main="主成分軸に合わせて回転された")
par(mfrow=c(1,1))



# 主成分の計算 ------------------------------------------------------------------


# 分散共分散行列
var(sample_data)

# 相関行列
cor(sample_data)

# 主成分の固有ベクトル
eigen_m <- eigen(cor(sample_data))
eigen_m$vectors
# 主成分分析の結果
pcr_model_sample$rotation


# データの回転と無相関化
sample_mat <- as.matrix(sample_data)

# 図示 
par(mfrow=c(1,2))
plot(
  sample_mat%*%eigen_m$vectors[,1], 
  sample_mat%*%eigen_m$vectors[,2], 
  main="固有ベクトルを使った回転"
)
plot(pcr_model_sample$x[,1], pcr_model_sample$x[,2], 
     main="主成分得点")
par(mfrow=c(1,1))



# アヤメデータの分析 ---------------------------------------------------------------


# アヤメの調査データ
summary(iris)

## データの図示
# install.packages("ggplot2")
# install.packages("GGally")
library(ggplot2)
library(GGally)
ggpairs(iris, aes_string(colour="Species", alpha=0.5))


## 主成分分析の実行
# 種類データを除く
pca_data <- iris[,-5]

# 主成分分析の実行
model_pca_iris <- prcomp(pca_data, scale=T)

# 結果
summary(model_pca_iris)


# 主成分分析の結果をグラフに描く
# install.packages("devtools")
# devtools::install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(
  model_pca_iris, 
  obs.scale = 1, 
  var.scale = 1, 
  groups = iris$Species, 
  ellipse = TRUE, 
  circle = TRUE,
  alpha=0.5
)


