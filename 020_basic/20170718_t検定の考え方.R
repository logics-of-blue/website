
# t検定の考え方| Logics of Blue
# https://logics-of-blue.com/t-test/
# 2017年07月18日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉


# Rによるt検定の実行 --------------------------------------------------------------

# データ
data <- data.frame(
  X = c(2, 0, 3, -3, 4, 1, -1, 4),
  Y = c(5, -1, 2, -1, 7, 3, 4, 5)
)

# データの中身
data

# 1群の検定
t.test(data$X)

# 対応のあるt検定
t.test(data$X, data$Y, paired=T)

# 母分散の比の検定
var.test(data$X, data$Y)

# 等分散の場合の平均値の差の検定
t.test(data$X, data$Y, var.equal = T)


# 不等分散の場合の平均値の差の検定
t.test(data$X, data$Y, var.equal = F)


