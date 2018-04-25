
# カイ二乗検定| Logics of Blue
# https://logics-of-blue.com/chi-squared-test/
# 2017年07月13日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉


# Rによるχ二乗検定 ---------------------------------------------------------------

# データを用意する
ABtest_data <- data.frame(
  button = c("blue", "blue", "red", "red"),
  result = c("press", "not","press", "not"),
  number = c(70, 180, 30, 120)
)

# データの中身
ABtest_data

# クロス集計表に変換
cross_data <- xtabs(number ~ ., ABtest_data)

# データの中身
cross_data

# χ二乗検定の実行
chisq.test(cross_data, correct=F)


# Fisherの正確確率検定 -----------------------------------------------------------


# Fisherの正確確率検定の実行
fisher.test(cross_data)


# シンプソンのパラドクス -------------------------------------------------------------

# データを用意する
paradox_data <- data.frame(
  button = c("blue", "blue", "red", "red"),
  result = c("press", "not","press", "not"),
  number = c(20, 60, 210, 250)
)

cross_paradox_data <- xtabs(number ~ ., paradox_data)

fisher.test(cross_paradox_data)






