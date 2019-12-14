
# Stanで状態空間モデルの収束を良くするコツ | Logics of Blue
# URL:  https://logics-of-blue.com/tips-for-better-convergence-when-making-state-space-model-with-stan
# 馬場真哉


# 分析の準備 ---------------------------------------------------------------------

library(tidyverse)
library(ggfortify)

library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



# シミュレーションデータの作成：ローカル線形トレンド----------------------------------------------------------

## パラメタの設定
N <- 200        # 期間
mu_0 <- 300     # 状態初期値
delta_0 <- -2   # ドリフト成分初期値

sd_w <- 2       # 水準成分の変動の大きさを表す標準偏差
sd_z <- 0.2     # ドリフト成分の変動の大きさを表す標準偏差
sd_v <- 20      # 観測誤差の標準偏差

## 乱数の生成
set.seed(1)          # 乱数の種
mu <- numeric(N)     # 状態(水準＋ドリフト)
delta <- numeric(N)  # 状態(ドリフト)
y <- numeric(N)      # 観測値

# 状態の1時点目の値を生成
mu[1] <- rnorm(n = 1, mean = mu_0, sd = sd_w)
delta[1] <- rnorm(n = 1, mean = delta_0, sd = sd_z)

# 状態の遷移と観測値の生成
for(i in 1:N){
  mu[i + 1] <- rnorm(n = 1, mean = mu[i] + delta[i], sd = sd_w)
  delta[i + 1] <- rnorm(n = 1, mean = delta[i], sd = sd_z)
  y[i] <- rnorm(n = 1, mean = mu[i], sd = sd_v)
}

# シミュレーションデータの時系列グラフ
y %>% ts() %>% autoplot(main = "シミュレーションデータ")


# データをlistにまとめる
data_list_1 <- list(T = N, y = y)


# 工夫をしない、素朴な実装 ------------------------------------------------------------

# ローカルレベルモデル
mod_ll <- stan(
  file = "local-level.stan",
  data = data_list_1,
  seed = 1
)

print(mod_ll, pars = c("s_w", "s_v"))

# 収束の確認
mcmc_rhat(rhat(mod_ll))

# 事後分布の可視化
mcmc_combo(mod_ll, pars = c("s_w", "s_v"))


# ローカル線形トレンドモデル
mod_llt <- stan(
  file = "local-linear-trend.stan",
  data = data_list_1,
  seed = 1
)

print(mod_llt, pars = c("s_w", "s_z", "s_v"))

# 収束の確認
mcmc_rhat(rhat(mod_llt))

# 事後分布の可視化
mcmc_combo(mod_llt, pars = c("s_w", "s_z", "s_v"))



# 収束を良くする工夫をした実装:ローカルレベルモデル----------------------------------------------------------

# ベクトル化
mod_ll_vec <- stan(
  file = "local-level-vec.stan",
  data = data_list_1,
  seed = 1
)

print(mod_ll_vec, pars = c("s_w", "s_v"))

# 収束の確認
mcmc_rhat(rhat(mod_ll_vec))

# 事後分布の可視化
mcmc_combo(mod_ll_vec, pars = c("s_w", "s_v"))


# 再パラメータ化
mod_ll_remodeling <- stan(
  file = "local-level-remodeling.stan",
  data = data_list_1,
  seed = 1
)

print(mod_ll_remodeling, pars = c("s_w", "s_v"))

# 収束の確認
mcmc_rhat(rhat(mod_ll_remodeling))

# 事後分布の可視化
mcmc_combo(mod_ll_remodeling, pars = c("s_w", "s_v"))


# adapt_deltaを増やす
mod_ll_remodeling_2 <- stan(
  file = "local-level-remodeling.stan",
  data = data_list_1,
  seed = 1,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

print(mod_ll_remodeling_2, pars = c("s_w", "s_v"))

# 収束の確認
mcmc_rhat(rhat(mod_ll_remodeling_2))

# 事後分布の可視化
mcmc_combo(mod_ll_remodeling_2, pars = c("s_w", "s_v"))


# 収束を良くする工夫をした実装:ローカル線形トレンドモデル----------------------------------------------------------

# 再パラメータ化
mod_llt_remodeling <- stan(
  file = "local-trend-remodeling.stan",
  data = data_list_1,
  seed = 1
)

# Rhatは1になった
print(mod_llt_remodeling, pars = c("s_w", "s_v"))

# 収束の確認
mcmc_rhat(rhat(mod_llt_remodeling))

# 事後分布の可視化
mcmc_combo(mod_llt_remodeling, pars = c("s_w", "s_v"))


# 参考：工夫をしないバージョン
print(mod_llt, pars = c("s_w", "s_z", "s_v"))
mcmc_combo(mod_llt, pars = c("s_w", "s_z", "s_v"))



# adapt_deltaを増やす
# 再パラメータ化
mod_llt_remodeling_2 <- stan(
  file = "local-trend-remodeling.stan",
  data = data_list_1,
  seed = 1,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

# 問題なし
print(mod_llt_remodeling_2, pars = c("s_w", "s_z", "s_v"))

# 収束の確認
mcmc_rhat(rhat(mod_llt_remodeling_2))

# 事後分布の可視化
mcmc_combo(mod_llt_remodeling_2, pars = c("s_w", "s_z", "s_v"))




