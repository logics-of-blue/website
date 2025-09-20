
# TMBを用いた状態空間モデルの推定


# ライブラリ読み込み ---------------------------------------------------------------

library(tidyverse)
library(KFAS)
library(TMB)
library(ggfortify) # autoplotのために使用
library(gridExtra) # 複数のグラフをまとめて表示させる


# データ生成 -------------------------------------------------------------------

# 乱数の種
set.seed(1)

# サンプルサイズ
n <- 100

# ローカルレベルモデルに従うシミュレーションデータ
mu <- rnorm(n = n, mean = 0, sd = 2) |> cumsum() |> ts()
y <- mu + rnorm(n = n, mean = 0, sd = 10) |> ts()

# 可視化
plot(y, type = "l")
lines(mu, col = 2, lwd = 2)


# KFAS(散漫カルマンフィルタ) --------------------------------------------------------

# Step1：モデルの構造を決める
build_kfas <- SSModel(
  H = NA,
  y ~ SSMtrend(degree = 1, Q = NA)
)

# Step2：パラメタ推定
fit_kfas <- fitSSM(build_kfas, inits = c(0, 0))

# Step3、4：フィルタリング・スムージング
result_kfas <- KFS(
  fit_kfas$model, 
  filtering = c("state", "mean"),
  smoothing = c("state", "mean")
)

# 観測誤差の標準偏差
fit_kfas$model$H |> sqrt()

# 過程誤差の標準偏差
fit_kfas$model$Q |> sqrt()

# 平滑化推状態
mu_smooth_kfas <- result_kfas$alphahat

# 可視化
plot_data <- 
  ts.union(
    y,             # 観測値 
    mu,            # 正しい状態
    mu_smooth_kfas # 状態推定値
  )
autoplot(plot_data, facets = FALSE) +
  scale_colour_manual(values = c("red", "green", "black"))


# TMB（ラプラス近似） -------------------------------------------------------------

# 以下のブログを参考にしました。
# https://ito4303.sakura.ne.jp/posts/2024-08-09-TMB-ssm/

# コンパイル
TMB::compile("ssm1.cpp")

# モデルのロード
dyn.load(dynlib("ssm1"))

# パラメータ推定の実施
dat <- list(Y = c(y))
pars <- list(mu = rep(0, length(y)), m0 = 0, log_s_w = 0, log_s_v = 0)
obj <- MakeADFun(dat, pars, random = "mu", DLL = "ssm1", silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)

# 推定結果
opt

# 推定された標準偏差
opt$par[1:2] |> exp()

# 推定されたパラメータの一覧
obj$env$parList()

# 推定されたパラメータの一覧(SE付き)
sdrep <- sdreport(obj)
summary(sdrep)

# 可視化
plot_data <- 
  ts.union(
    y,                    # 観測値 
    mu,                   # 正しい状態
    mu_smooth_kfas,       # 状態推定値(KFAS)
    obj$env$parList()$mu  # 状態推定値(TMB)
  )
colnames(plot_data) <- c("y", "mu", "KFAS", "TMB")

autoplot(plot_data, facets = FALSE) +
  scale_colour_manual(values = c("green", "red", "blue", "black"))


# TMBでDGLM（二項分布） ----------------------------------------------------------

# 乱数の種
set.seed(1)

# 逆ロジット
logistic <- function(x) {
  1 / (1 + exp(-x))
}

# サンプルサイズ
n <- 100

# シミュレーションデータ
mu <- -2 + rnorm(n = n, mean = 0, sd = 0.4) |> cumsum() |> ts()
y <- numeric(n)

for (t in 1:n) {
  y[t] <- rbinom(n = 1, size = 1, prob = logistic(mu[t]))
}

# 可視化
plot(y)
lines(logistic(mu), col = 2, lwd = 2)



# コンパイル
TMB::compile("ssm2.cpp")

# モデルのロード
dyn.load(dynlib("ssm2"))

# パラメータ推定の実施
dat <- list(Y = c(y))
pars <- list(mu = rep(0, length(y)), m0 = 0, log_s_w = 0)
obj <- MakeADFun(dat, pars, random = "mu", DLL = "ssm2", silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)

# 推定結果
opt

# 推定された標準偏差
opt$par[1] |> exp()

# 推定された状態
sdrep <- sdreport(obj)
summary(sdrep)
summary(sdrep, select = "random")
summary(sdrep, select = "report")

# 可視化
plot(y)
lines(logistic(mu), col = 2, lwd = 2)
summary(sdrep, select = "report")[, 1] |> 
  ts() |> 
  lines(col = 3, lwd = 2)


# TMBでランダム効果付きポアソンDLM ---------------------------------------------------------

# 乱数の種
set.seed(1)

# サンプルサイズ
n <- 100

# シミュレーションデータ
mu <- -2 + rnorm(n = n, mean = 0, sd = 0.4) |> cumsum() |> ts()
r <- rnorm(n = n, mean = 0, sd = 0.5)

y <- numeric(n)

for (t in 1:n) {
  y[t] <- rpois(n = 1, lambda = exp(mu[t] + r[t]))
}

# 可視化
plot(y)
lines(exp(mu), col = 2, lwd = 2)

# コンパイル
TMB::compile("ssm3.cpp")

# モデルのロード
dyn.load(dynlib("ssm3"))

# パラメータ推定の実施
dat <- list(Y = c(y))
pars <- list(
  mu = rep(0, length(y)), m0 = 0, 
  r = rep(0, length(y)), 
  log_s_w = -2, log_s_r = -1)
obj <- MakeADFun(
  dat, pars, random = c("mu", "r"), 
  DLL = "ssm3", silent = TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)

# 推定結果
opt

# 推定された標準偏差
opt$par[1:2] |> exp()

# 推定された状態
sdrep <- sdreport(obj)
summary(sdrep)
summary(sdrep, select = "random")
summary(sdrep, select = "report")

# 可視化
plot(y)
lines(exp(mu), col = 2, lwd = 2)
summary(sdrep, select = "report")[, 1] |> 
  ts() |> 
  lines(col = 3, lwd = 2)


