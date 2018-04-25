
# カルマンフィルタと最尤法 | Logics of Blue
# https://logics-of-blue.com/kalman-filter-mle/
# 2017年04月16日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉


# ライブラリを使わないカルマンフィルタの実装 ---------------------------------------------------

# 尤度を計算するための情報も出力するようにした
localLevelModel_2 <- function(y, xPre, pPre, sigmaW, sigmaV) {
  
  #状態の予測(ローカルレベルモデルなので、予測値は、前期の値と同じ)
  xForecast <- xPre
  
  # 状態の予測誤差の分散
  pForecast <- pPre + sigmaW
  
  # 観測値の予測誤差
  v <- y - xForecast
  
  # 観測値の予測誤差の分散
  F <- pForecast + sigmaV
  
  # カルマンゲイン
  kGain <- pForecast / (pForecast + sigmaV)
  
  # カルマンゲインを使って補正された状態
  xFiltered <- xForecast + kGain * (y - xForecast)
  
  # 補正された状態の誤差の分散
  pFiltered <- (1 - kGain) * pForecast
  
  # 結果の格納
  result <- data.frame(
    xFiltered = xFiltered, 
    pFiltered = pFiltered,
    v = v,
    F = F
  )
  
  return(result)
}




# ナイル川流量データで計算
Nile

# サンプルサイズ
N <- length(Nile)

# 状態の推定値
x <- numeric(N)	

# 状態の予測誤差の分散
P <- numeric(N)	


# 「状態」の初期値は0とします
x <- c(0, x)
x

# 「状態の予測誤差の分散」の初期値は10000000にします
P <- c(10000000, P)
P


# 観測値の予測誤差
v <- numeric(N)

# 観測値の予測誤差の分散
F <- numeric(N)


# カルマンフィルタの逐次計算を行う
for(i in 1:N) {
  kekka <- localLevelModel_2(Nile[i],x[i],P[i], sigmaW = 1000, sigmaV = 10000)
  x[i + 1] <- kekka$xFiltered
  P[i + 1] <- kekka$pFiltered
  v[i] <- kekka$v
  F[i] <- kekka$F
}


# 観測値の予測誤差の系列
v

# 観測値の予測誤差の分散の時系列
F


# 対数尤度の計算1
# dnorm関数を使った対数尤度の計算
sum(log(dnorm(v, mean = 0, sd = sqrt(F))))

# 対数尤度の計算 2
# 教科書通りの計算式を使った、対数尤度の計算
-1 * (N/2) * log(2 * pi) - 1/2 * sum(log(F) + v^2 / F)

# 対数尤度の計算 3
# dlmパッケージで計算される対数尤度
1/2 * sum(log(F) + v^2 / F)


# dlmでの計算結果との比較
# dlmパッケージの読み込み
library(dlm)

# dlmのパラメタの設定
modelDlm <- dlmModPoly(order=1, m0=0, C0=10000000,  dW = 1000,    dV = 10000)

# カルマンフィルタの実行
Filter <- dlmFilter(Nile, modelDlm)


# 対数尤度の計算
dlmLL(Nile, modelDlm)




# 最尤法の実行 ------------------------------------------------------------------


calcLogLikLocalLevel <- function(sigma){
  # パラメタ（状態・観測方程式のノイズの分散）を入れると、すぐに対数尤度を計算してくれる関数
  
  # データや、「状態」「状態の予測誤差の分散」の初期値の設定
  data <- Nile
  x0 <- 0
  P0 <- 10000000
  
  # 分散は負にならないため、あらかじめEXPをとっておく
  sigmaW <- exp(sigma[1])
  sigmaV <- exp(sigma[2])
  
  # サンプルサイズ
  N <- length(data)
  
  # 状態の予測誤差の分散
  P <- numeric(N)	
  
  # 「状態の予測誤差の分散」の初期値の設定
  P <- c(P0, P)
  
  # 状態の推定値
  x <- numeric(N)	
  
  # 「状態」の初期値の設定
  x <- c(x0, x)
  
  # 観測値の予測誤差の系列
  v <- numeric(N)
  
  # 観測値の予測誤差の分散の時系列
  F <- numeric(N)
  
  # カルマンフィルタの逐次計算を行う
  for(i in 1:N) {
    kekka <- localLevelModel_2(Nile[i],x[i],P[i], sigmaW = sigmaW, sigmaV = sigmaV)
    x[i + 1] <- kekka$xFiltered
    P[i + 1] <- kekka$pFiltered
    v[i] <- kekka$v
    F[i] <- kekka$F
  }
  
  # 対数尤度を返す
  return(1/2 * sum(log(F) + v^2 / F))
  
}

# 動作確認
calcLogLikLocalLevel(sigma=c(log(1000),log(10000)))

# 最尤法の実行
optim(c(1,1), calcLogLikLocalLevel, method = "L-BFGS-B")


# dlmパッケージを使ったパラメタの推定
buildDlm <- function(theta){
  dlmModPoly(order=1, m0=0, C0=10000000, dW=exp(theta[1]), dV=exp(theta[2]))
}
fitDlm <- dlmMLE(Nile, parm=c(1, 1), buildDlm)

# dlmパッケージでの計算結果
fitDlm
exp(fitDlm$par)



