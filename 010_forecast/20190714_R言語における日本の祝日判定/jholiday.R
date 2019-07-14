is.jholiday <- function(target_date, 
                        holiday_source = "https://www8.cao.go.jp/chosei/shukujitsu/syukujitsu.csv",
                        excluded_days = NULL,
                        flag = FALSE) {
  # 日本の祝日を判定する関数。
  # 初期状態では、『内閣府：「国民の祝日」について』というWebサイトで
  # 提供されている祝日のデータを用いて祝日判定を行う。
  # この場合はネット環境が必要。
  #
  # Args:
  #   target_date     : 祝日判定の対象となる日付のベクトル
  #   holiday_source  : 祝日判定の根拠となる祝日データ
  #                     1列目が祝日の日付、2列目が祝日の名称
  #   excluded_days   : 除外する曜日のベクトル。その曜日は決して祝日とみなされない
  #                     「月・火・水・木・金・土・日」のいずれかを指定する
  #   flag            : TRUEなら、0または1の数値型のベクトルを返す
  #                     FALSEなら、論理値型のベクトルを返す
  #
  # Returns:
  #   祝日判定の結果。target_dateと同じ長さのベクトル
  
  # 対象となるベクトルを日付型にする
  target_date <- as.Date(target_date)
  
  # holiday_sourceをもとに祝日を取得
  holidays <- read.csv(holiday_source)
  colnames(holidays) <- c("date", "holyday_name")
  holidays$date <- as.Date(holidays$date)

  # 祝日判定
  if(is.null(excluded_days)) {
    # すべての曜日で判定
    result <- target_date %in% holidays$date
  } else {
    # 特定の曜日を除外
    result <- (target_date %in% holidays$date) & !(weekdays(target_date, TRUE) %in% excluded_days)
  }
  
  if(flag) {
    # 0または1のフラグにする
    result <- as.numeric(result)
  }
  
  # 祝日判定の結果を返す
  return(result)
}

