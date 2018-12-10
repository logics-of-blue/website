
# Stanによる多変量時系列モデル | Logics of Blue
# https://logics-of-blue.com/multivariate-time-series-with-stan/
# 馬場真哉


# 分析の準備 ---------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(ggfortify)
library(gghighlight)

library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# モデル1：パラメタ設定 --------------------------------------------------------------

### シミュレーションデータの作成

## パラメタの設定
N <- 100       # 期間
user_num <- 10 # ユーザー数
x_0 <- 200     # 状態初期値
s_w <- 20      # 過程誤差の標準偏差
s_v <- 30      # 観測誤差の標準偏差
s_r <- 50      # ランダム効果の標準偏差

x <- matrix(nrow = N)                  # 全ユーザー共通の状態
r <- matrix(ncol = user_num)           # ユーザーごとのランダム効果
y <- matrix(nrow = N, ncol = user_num) # ユーザーごとの売り上げ

# わかりやすくするため、列名を付けておく
colnames(x) <- "state"
colnames(y) <- LETTERS[1:10]
colnames(r) <- LETTERS[1:10]


# 状態
head(x, n = 3)
# 観測値
head(y, n = 3)
# ユーザー別の売り上げ増減効果
r


# モデル1：データ作成 --------------------------------------------------------------

# 乱数の種
set.seed(2)

# ユーザーごとのランダム効果の生成
r[] <- rnorm(n = user_num, mean = 0, sd = s_r)

r %>% round(1)

# 状態の初期値
x[1,] <- rnorm(n = 1, mean = x_0, sd = s_w)

# 各時点（i）ごとにループを回して、状態を遷移させる
for(i in 2:N){
  # 状態の遷移（ランダムウォーク仮定）
  x[i,] <- rnorm(n = 1, mean = x[i-1], sd = s_w)
}

# 各時点（i）ごと、ユーザー（j）ごとにループを回す
for(i in 1:N){
  for(j in 1:user_num){
    # 「状態＋ユーザー固有のランダム効果」を平均値として、
    # 観測ノイズが加わって、観測値が得られる
    y[i,j] <- rnorm(n = 1, mean = x[i]+r[j], sd = s_v)
  }
}

# 状態
x %>% round(1) %>% head(n = 3)

# 10人の売り上げ時系列データ
y %>% round(1) %>% head(n = 3)


# モデル１：整形 -----------------------------------------------------------------

# データをまとめる
sim_df <- y %>% 
  cbind(state = x) %>% 
  cbind(time = 1:100) %>% 
  data.frame

# 観測値・状態・時点番号をまとめた結果
sim_df %>% round(1) %>% head(n = 3)


# 整然データの形式にする
sim_tidy <- sim_df %>% 
  gather(key = "name",
         value = "sales",
         factor_key = TRUE,
         - time)

# 結果
summary(sim_tidy)
summary(sim_tidy$name)
head(sim_tidy, n = 3)

# 図示
ggplot(data = sim_tidy) + 
  ggtitle("シミュレーションにより生成された売り上げデータ（モデル１）") +
  geom_line(aes(x = time, y = sales, color = name)) + 
  gghighlight(name == "state" | name == "A"| name == "C", 
              use_group_by = FALSE)



# モデル１：推定 -----------------------------------------------------------------

# データの準備
data_list_1 <- list(
  y = y, 
  T = N,
  user_num = user_num
)

data_list_1

# 多変量モデルの推定
malti_1 <- stan(
  file = "ssm-user-sales.stan",
  data = data_list_1,
  seed = 1, 
  iter = 30000,
  warmup = 10000,
  thin = 10
)

# 収束の確認
mcmc_rhat(rhat(malti_1))
check_hmc_diagnostics(malti_1)

# モデル1の推定結果
print(malti_1, 
      par = c("s_w", "s_v", "s_r", "lp__"),
      probs = c(0.025, 0.5, 0.975))


# 推定結果一覧
options(max.print = 7000)
print(malti_1, probs = c(0.025, 0.5, 0.975))



# モデル１：図示 -----------------------------------------------------------------

# データの整形
stan_df_1 <- malti_1 %>% 
  rstan::extract() %$% x %>% 
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>% 
  t() %>% 
  cbind(1:nrow(sim_df)) %>% 
  data.frame 

# 列名の変更
colnames(stan_df_1) <- c("lwr", "fit", "upr", "time")

# 結果
head(stan_df_1, n = 3) 


# 図示
ggplot(data = sim_tidy) + 
  ggtitle("推定結果（モデル１）") +
  geom_line(aes(x = time, y = sales, color = name)) + 
  gghighlight(name == "state", use_group_by = FALSE) + 
  geom_line(data = stan_df_1, 
            aes(x = time, y = fit), size = 1.2) +
  geom_ribbon(data = stan_df_1, 
              aes(x = time, ymin = lwr, ymax = upr), alpha = 0.3)



## SSMを使わなくても、平均をとるだけでよい？

# 状態と比較
cbind(y_mean = apply(y, 1,mean), 
      state = x, 
      fit = stan_df_1[,"fit"]) %>% 
  ts() %>% 
  autoplot(facet = F, main = "状態推定値と平均値の比較")




# モデル２：パラメタ設定 -----------------------------------------

# 個人ごとに異なる広告をうった

## パラメタの設定
N <- 100       # 期間
user_num <- 10 # ユーザー数
x_0 <- 200     # 状態初期値
s_w <- 20      # 過程誤差の標準偏差
s_v <- 30      # 観測誤差の標準偏差
s_r <- 50      # ランダム効果の標準偏差

b_ad <- 100     # 広告による売り上げ増加効果


x <- matrix(nrow = N)                        # 全ユーザー共通の状態
r <- matrix(ncol = user_num)                 # ユーザーごとのランダム効果
y <- matrix(nrow = N, ncol = user_num)       # ユーザーごとの売り上げ
ad_flag <- matrix(nrow = N, ncol = user_num) # 0なら広告なし。1で広告あり。

# わかりやすくするため、列名を付けておく
colnames(x) <- "state"
colnames(y) <- LETTERS[1:10]
colnames(ad_flag) <- LETTERS[1:10]
colnames(r) <- LETTERS[1:10]


# 状態
head(x, n = 3)
# 観測値
head(y, n = 3)
# ユーザー別の売り上げ増減効果
r
# ユーザー別の広告送付実績
head(ad_flag, n = 3)

# モデル２：データ作成 -----------------------------------------

# 乱数の種
set.seed(2)

# ユーザーごとのランダム効果の生成
r[] <- rnorm(n = user_num, mean = 0, sd = s_r)

# AさんとDさんは売り上げがとても低い
# HさんとJさんもちょっと低い
round(r, 1)

# 全体的にランダムに広告をいれる
ad_flag[1:50,] <- sample(c(0,1), 
                  size = N*user_num / 2, 
                  replace = T, 
                  prob = c(0.7, 0.3))

# 広告配信率
mean(ad_flag[1:50,])

# 最後の50日は、A,D,H,Jさんはずっと広告を打つ。他は広告なし
ad_flag[51:100,] <- 0
ad_flag[51:100, c("A", "D", "H", "J")] <- 1

ad_flag


# 乱数の種
set.seed(2)

# 状態の初期値
x[1, ] <- rnorm(n = 1, mean = x_0, sd = s_w)

# 各時点（i）ごとにループを回して、状態を遷移させる
for(i in 2:N){
  # 状態の遷移（ランダムウォーク仮定）
  x[i,] <- rnorm(n = 1, mean = x[i-1], sd = s_w)
}

# 各時点（i）ごと、ユーザー（j）ごとにループを回す
for(i in 1:N){
  for(j in 1:user_num){
    # 「状態＋ユーザー固有のランダム効果＋広告効果」を平均値として、
    # 観測ノイズが加わって、観測値が得られる
    y[i,j] <- rnorm(n = 1, mean = x[i]+r[j]+ad_flag[i, j]*b_ad, sd = s_v)
  }
}

# 10人の売り上げ時系列データ
y %>% round(1) %>% head(n = 3)



# モデル２：整形 -----------------------------------------------------------------

# 整然データの形式にする
sim_tidy <- y %>% 
  cbind(state = x) %>% 
  cbind(time = 1:100) %>% 
  data.frame %>% 
  gather(key = "name",
         value = "sales",
         factor_key = TRUE,
         - time)

# 結果
summary(sim_tidy$name)
head(sim_tidy, n = 3)

# 図示
ggplot(data = sim_tidy) + 
  ggtitle("シミュレーションにより生成された売り上げデータ（モデル２）") +
  geom_line(aes(x = time, y = sales, color = name)) + 
  gghighlight(name == "state" | name == "D"| name == "I", 
              use_group_by = FALSE)



# モデル２：推定 -----------------------------------------------------------------

# データの準備
data_list_2 <- list(
  y = y, 
  ad_flag = ad_flag,
  T = N,
  user_num = user_num
)

# 多変量モデルの推定
malti_2 <- stan(
  file = "ssm-user-sales-ad.stan",
  data = data_list_2,
  seed = 1,
  iter = 30000,
  warmup = 10000,
  thin = 10
)

# 収束の確認
mcmc_rhat(rhat(malti_2))
check_hmc_diagnostics(malti_2)


# 多変量モデルの推定結果
print(malti_2, 
      par = c("s_w", "s_v", "s_r", "b_ad", "lp__"),
      probs = c(0.025, 0.5, 0.975))

# 推定結果一覧
options(max.print = 7000)
print(malti_2, probs = c(0.025, 0.5, 0.975))

# トレースプロット
traceplot(malti_2, par = c("s_w", "s_v", "s_r", "b_ad", "lp__"))
traceplot(malti_2, par = c("x[1]", "x[50]", "x[100]"))
traceplot(malti_2, par = c("r[1]", "r[5]", "r[10]"))


# モデル２：図示 -----------------------------------------------------------------

# データの整形
stan_df_2 <- malti_2 %>% 
  rstan::extract() %$% x %>% 
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>% 
  t() %>% 
  cbind(1:nrow(sim_df)) %>% 
  data.frame 

# 列名の変更
colnames(stan_df_2) <- c("lwr", "fit", "upr", "time")

# 図示
ggplot(data = sim_tidy) + 
  ggtitle("推定結果（モデル１）") +
  geom_line(aes(x = time, y = sales, color = name)) + 
  gghighlight(name == "state", use_group_by = FALSE) + 
  geom_line(data = stan_df_2, 
            aes(x = time, y = fit), size = 1.2) +
  geom_ribbon(data = stan_df_2, 
              aes(x = time, ymin = lwr, ymax = upr), alpha = 0.3)



## 平均値よりも、状態を正しく反映してそうに見える

# 状態と比較
cbind(y_mean = apply(y, 1,mean), 
      state = x, 
      fit = stan_df_2[,"fit"]) %>% 
  ts() %>% 
  autoplot(facet = F, main = "状態推定値と平均値の比較")







