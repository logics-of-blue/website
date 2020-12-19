
# 分散分析の基礎 | Logics of Blue
# https://logics-of-blue.com/anova-foundation/



# 分散分析の実行 -----------------------------------------------------------------


# データの読み込み
data <- read.csv("data.csv")
data

# モデル化
anova_mod <- lm(weight ~ food, data = data)

# 分散分析表
anova_table <- anova(anova_mod)
anova_table

# F分布を使ってp値を計算する
1 - pf(16, 2, 3)


# 分散分析表はdata.frameなので、結果の取り出しは容易
class(anova_table)

# F比
anova_table["F value"]
anova_table["food", "F value"]


# PB検定 --------------------------------------------------------------------

# foodによって体重が変わらないことを仮定したモデル
anova_null <- lm(weight ~ 1, data = data)

# 推定されたパラメータ
coef(anova_null)

# 総平均
mean(data$weight)

# モデルの標準偏差
summary(anova_null)$sigma

# 体重の標準偏差
sd(data$weight)

# 帰無仮説が正しいと仮定して、データを生成する
simulate(anova_null, n_sim = 1)
simulate(anova_null, n_sim = 1)

# 乱数を固定する
simulate(anova_null, n_sim = 1, seed = 1)
simulate(anova_null, n_sim = 1, seed = 1)

# 平均7、標準偏差3.741657の正規分布に従う確率変数を生成しているのと同じ
set.seed(1)
mu <- mean(data$weight)
sd <- sd(data$weight)
size <- nrow(data)
rnorm(n = size, mean = mu, sd = sd)

# 同じ結果になっていることの確認
res_simulate <- simulate(anova_null, n_sim = 1, seed = 1)
set.seed(1)
res_rnorm <- rnorm(n = size, mean = mu, sd = sd)
round(res_simulate, 5) == round(res_rnorm, 5)


# このシミュレーションデータに対してF比を計算すると、
# 小さなF比が得られやすい
set.seed(1)
res_rnorm <- rnorm(n = size, mean = mu, sd = sd)
mod_sim <- lm(res_rnorm ~ data$food)
anova(mod_sim)



# PB検定の実行

n_sim <- 50000                # シミュレーションする回数
f_ratio_vec <- numeric(n_sim) # F比を保管する容れ物
exp_vec <- data$food          # 水準

set.seed(1)
for (i in 1:n_sim) {
  # シミュレーションにより体重データを生成
  # このデータは、体重が餌によって変化しないことを想定している
  simlated_weight <- rnorm(n = size, mean = mu, sd = sd)
  # モデル化と分散分析表の出力
  mod_sim <- lm(simlated_weight ~ exp_vec)
  table_sim <-  anova(mod_sim)
  # F比を保管
  f_ratio_vec[i] <- table_sim["exp_vec", "F value"]
}

# F比が16以上である割合
sum(f_ratio_vec >= 16) / n_sim





