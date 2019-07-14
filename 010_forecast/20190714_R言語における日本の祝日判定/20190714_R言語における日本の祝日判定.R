
# R言語における日本の祝日判定 | Logics of Blue
# https://logics-of-blue.com/determination-of-japanese-national-holidays-with-r/
# 馬場真哉


# 基本的な使い方 -----------------------------------------------------------------

# 関数の読み込み
source("https://raw.githubusercontent.com/logics-of-blue/website/master/010_forecast/20190714_R%E8%A8%80%E8%AA%9E%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E6%97%A5%E6%9C%AC%E3%81%AE%E7%A5%9D%E6%97%A5%E5%88%A4%E5%AE%9A/jholiday.R", encoding="utf-8")

# 祝日判定をしたい日付データ
target_date <- seq(
  from = as.Date("2019-05-01"), 
  to = as.Date("2019-5-31"),
  by = 1
)

target_date

# 日付において、祝日ならTRUE、そうでなければFALSEを返す
is.jholiday(target_date = target_date)


# 「時系列分析と状態空間モデルの基礎」での対応 -----------------------------------------------------------------------

# パッケージの読み込み
library(KFAS)
library(xts)
library(ggplot2)
library(ggfortify)
library(gridExtra)

# Nipponパッケージの代わりに、祝日判定関数を読み込む
source("https://raw.githubusercontent.com/logics-of-blue/website/master/010_forecast/20190714_R%E8%A8%80%E8%AA%9E%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E6%97%A5%E6%9C%AC%E3%81%AE%E7%A5%9D%E6%97%A5%E5%88%A4%E5%AE%9A/jholiday.R", encoding="utf-8")

# データの読み込み
file_data <- read.csv("https://raw.githubusercontent.com/logics-of-blue/book-tsa-ssm-foundation/master/book-data/5-11-sales_data.csv")

# xts型に変換
sales <- as.xts(read.zoo(file_data))
head(sales, n = 3)

# 日付の抽出
dates <- index(sales)
head(dates, n = 5)

# 祝日の判定
head(is.jholiday(dates))

## 以下略(書籍とまったく同じコードでOKです)


# ネットに接続できない場合 ---------------------------------------------------------------

# ローカルに、例えば『C:\data_folder』に『jholyday.R』と『syukujitsu.csv』があるならば

# 祝日判定関数の読み込み
source("C:/data_folder/jholiday.R", encoding="utf-8")

# 祝日判定をしたい日付データ
target_date <- seq(
  from = as.Date("2019-05-01"), 
  to = as.Date("2019-5-31"),
  by = 1
)

# 祝日判定
is.jholiday(target_date = target_date,
            holiday_source = "C:/data_folder/syukujitsu.csv")



# 特定の曜日を除く ----------------------------------------------------------------

# 日曜日を除く
is.jholiday(target_date = target_date,
            excluded_days = "日")

# 土日を除く
is.jholiday(target_date = target_date,
            excluded_days = c("土", "日"))


# 0か1の祝日フラグを作る ------------------------------------------------------------------

# 0か1のフラグで出力
is.jholiday(target_date = target_date,
            flag = TRUE)


# 日曜日を除き、0か1のフラグで出力
is.jholiday(target_date = target_date,
            excluded_days = "日",
            flag = TRUE)





