
# 時系列データへの回帰分析| Logics of Blue
# https://logics-of-blue.com/time-series-regression/
# 2017年05月07日：新規作成
# 2018年04月25日：コードを一部修正し、動作確認
# 馬場真哉


# パッケージの読み込み --------------------------------------------------------------

# 使用するパッケージ一覧
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("tseries")
# install.packages("urca")
# install.packages("lmtest")
# install.packages("nlme")

library(ggplot2)
library(reshape2)
library(tseries)
library(urca)
library(lmtest)
library(nlme)


# シミュレーションによる回帰係数のp値の確認 ---------------------------------------------------


# 正しく検定できている場合

# シミュレーションの回数など
Nsim <- 200
Nsample <- 400

# p値を格納する変数
pValues <- numeric(Nsim)

# シミュレーションの実行
set.seed(1)
for(i in 1:Nsim){
  # 自己相関のないシミュレーションデータ
  y <- rnorm(Nsample, sd = sqrt(10))
  x <- rnorm(Nsample, sd = sqrt(10))
  
  # 線形回帰分析の実行
  mod <- lm(y ~ x)
  
  # p値を保存
  pValues[i] <- summary(mod)$coefficients[2,4]
}


# シミュレーションによる、見せかけの回帰の確認：単位根過程の場合 ------------------------------

# p値を格納する変数
pValuesRW <- numeric(Nsim)

# シミュレーションの実行
set.seed(1)
for(i in 1:Nsim){
  # ランダムウォークするシミュレーションデータ
  y <- cumsum(rnorm(n=Nsample))
  
  x <- cumsum(rnorm(n=Nsample))
  
  # 線形回帰分析の実行
  mod <- lm(y ~ x)
  
  # p値を保存
  pValuesRW[i] <- summary(mod)$coefficients[2,4]
  
}


# データを結合
simResult <- data.frame(
  pValues = c(pValues, pValuesRW),
  simPattern = rep(c("正しい回帰", "見せかけの回帰"), each = Nsim)
)

# ヒストグラムの描画
library(ggplot2)
histPlot <- ggplot(
  simResult, 
  aes(
    x = pValues,
    fill = simPattern
  )
)

histPlot <- histPlot + geom_histogram(
  alpha = 0.5,
  position = "identity",
  binwidth  = 0.1
)

plot(histPlot)



# 単位根検定 -------------------------------------------------------------------


# ランダムウォークするシミュレーションデータ
set.seed(1)
y <- cumsum(rnorm(n=Nsample))
x <- cumsum(rnorm(n=Nsample))

# install.packages("tseries")
library(tseries)

# 帰無仮説は「単位根過程である」
# 対立仮説は「単位根じゃない」
# 帰無仮説を棄却できないので、「単位根ではないと主張することが難しい」状態。
adf.test(y)
adf.test(x)


# 差分系列への回帰分析 --------------------------------------------------------------

# p値を格納する変数
pValuesDiff <- numeric(Nsim)

# シミュレーションの実行
set.seed(1)
for(i in 1:Nsim){
  # ランダムウォークするシミュレーションデータ
  y <- cumsum(rnorm(n=Nsample))
  
  x <- cumsum(rnorm(n=Nsample))
  
  # 線形回帰分析の実行
  mod <- lm(y ~ x)
  
  # 差分系列をとる
  yDiff <- diff(y)
  xDiff <- diff(x)
  
  modDiff <- lm(yDiff ~ xDiff)
  pValuesDiff[i] <- summary(modDiff)$coefficients[2,4]
}

# データを結合
simResult <- data.frame(
  pValues = c(pValues, pValuesRW, pValuesDiff),
  simPattern = rep(c("正しい回帰", "見せかけの回帰", "差分系列への回帰"), each = Nsim)
)

# ヒストグラムの描画
histPlot <- ggplot(
  simResult, 
  aes(
    x = pValues,
    fill = simPattern
  )
)

histPlot <- histPlot + geom_histogram(
  alpha = 0.5,
  position = "identity",
  binwidth  = 0.1
)

plot(histPlot)



# 共和分検定：Engle-Grangerの方法 --------------------------------------------------


# install.packages("urca")
library(urca)

Nsim <- 200
Nsample <- 400

# シミュレーションデータの作成
set.seed(1)
y <- cumsum(rnorm(n=Nsample))
x <- cumsum(rnorm(n=Nsample))

# データをmatrix形式にまとめる
dataM <- matrix(nrow = Nsample, ncol = 2)
dataM[,1] <- y
dataM[,2] <- x

# PO検定の実施
summary(ca.po(dataM, demean = "none"))





# 参考：共和分となっているデータを作って、共和分検定をしてみる ------------------------------------------

# シミュレーションデータの作成
set.seed(1)
RW <- cumsum(rnorm(n=Nsample))
x2 <- 0.6 * RW + rnorm(n=Nsample)
y2 <- 0.4 * RW + rnorm(n=Nsample)

# 両方ともに単位根であることを棄却できない
adf.test(y2)
adf.test(x2)

# グラフを見ると、やはりランダムウォークに見える
# ただし特定の係数をかけてから和をとると、ランダムウォークには見えなくなる
df <- data.frame(
  id = 1:Nsample,
  y2 = y2,
  x2 = x2,
  sum = x2 - (0.6/0.4)*y2
)
df <- melt(df, id = "id")


linePlot <- ggplot(
  df,
  aes(
    x = id,
    y = value,
    group = variable,
    colour = variable
  )
) + geom_line()

plot(linePlot)


# 共和分検定
dataM2 <- matrix(nrow = Nsample, ncol = 2)
dataM2[,1] <- y2
dataM2[,2] <- x2
summary(ca.po(dataM2, demean = "none"))


# 共和分のあるデータに、差分をとってから回帰すると、関係が見えなくなっていしまう
y2Diff <- diff(y2)
x2Diff <- diff(x2)

modLmDiffDame <- lm(y2Diff ~ x2Diff)
summary(modLmDiffDame)




# シミュレーションによる、見せかけの回帰の確認：定常過程の場合 ------------------------------------------

# シミュレーションの回数など
Nsim <- 200
Nsample <- 400

# p値を格納する変数
pValuesAutoCol <- numeric(Nsim)

# AICを格納する変数
AIClm <- numeric(Nsim)
AICnull <- numeric(Nsim)

# シミュレーションの実行
set.seed(1)
for(i in 1:Nsim){
  # 自己相関のあるシミュレーションデータ
  y <- arima.sim(
    n = Nsample,sd = sqrt(10),
    model = list(order = c(1,0,0), ar = c(0.8))
  )
  
  x <- arima.sim(
    n = Nsample,sd = sqrt(10),
    model = list(order = c(1,0,0), ar = c(0.8))
  )
  
  # 線形回帰分析の実行
  mod <- lm(y ~ x)

  # p値を保存
  pValuesAutoCol[i] <- summary(mod)$coefficients[2,4]
  
  # AIC
  AIClm[i] <- AIC(mod)
  AICnull[i] <- AIC(lm(y ~ 1))
}

# データを結合
simResult <- data.frame(
  pValues = c(pValues, pValuesAutoCol),
  simPattern = rep(c("正しい回帰", "自己相関あり"), each = Nsim)
)

head(simResult)

# ヒストグラムの描画
histPlot <- ggplot(
  simResult, 
  aes(
    x = pValues,
    fill = simPattern
  )
)

histPlot <- histPlot + geom_histogram(
  alpha = 0.5,
  position = "identity",
  binwidth  = 0.1
)

plot(histPlot)


# AICによるモデル選択もダメ
sum(AIClm < AICnull) / Nsim


# 残差の自己相関の検定 --------------------------------------------------------------

# 乱数の種
set.seed(2)

# 自己相関のあるシミュレーションデータ
y <- arima.sim(
  n = Nsample,sd = sqrt(10),
  model = list(order = c(1,0,0), ar = c(0.8))
)

x <- arima.sim(
  n = Nsample,sd = sqrt(10),
  model = list(order = c(1,0,0), ar = c(0.8))
)

autoCorData <- data.frame(
  y = y,
  x = x
)

# 普通の回帰分析
lmModel <- lm(y ~ x, data = autoCorData)

summary(lmModel)


# ダービンワトソン検定により、系列相関の有無を検定する
# 必要なら、パッケージをインストールします
# install.packages("lmtest")
library(lmtest)

dwtest(lmModel)


# 一般化最小二乗法の実行 -------------------------------------------------------------


# install.packages("nlme")
library(nlme)

# 次数を調べる
glsModel_0 <- gls(y ~ x, correlation =  NULL         , data = autoCorData)
glsModel_1 <- gls(y ~ x, correlation = corARMA(p = 1), data = autoCorData)
glsModel_2 <- gls(y ~ x, correlation = corARMA(p = 2), data = autoCorData)

# AR(1)がAIC最小となった
AIC(
  glsModel_0,
  glsModel_1,
  glsModel_2
)

# 尤度比検定もできる。こちらもAR(1)が選ばれた
anova(glsModel_0, glsModel_1)
anova(glsModel_1, glsModel_2)
anova(glsModel_0, glsModel_2)

# 推定結果
summary(glsModel_1)





# 一般化最小二乗法のシミュレーション -------------------------------------------------------

# p値を格納する変数
pValuesGls <- numeric(Nsim)

# シミュレーションの実行
set.seed(1)
for(i in 1:Nsim){
  # 自己相関のあるシミュレーションデータ
  y <- arima.sim(
    n = Nsample,
    model = list(order = c(1,0,0), ar = c(0.8)),
    sd = sqrt(10)
  )
  
  x <- arima.sim(
    n = Nsample,
    model = list(order = c(1,0,0), ar = c(0.8)),
    sd = sqrt(10)
  )
  
  # 線形回帰分析の実行
  mod <- gls(y ~ x, correlation = corARMA(p = 1))
  
  # p値を保存
  pValuesGls[i] <- summary(mod)$tTable[2,4]
}



# データを結合
simResult <- data.frame(
  pValues = c(pValues, pValuesAutoCol, pValuesGls),
  simPattern = rep(c("正しい回帰", "自己相関あり", "GLS"), each = Nsim)
)

head(simResult)

# ヒストグラムの描画
histPlot <- ggplot(
  simResult, 
  aes(
    x = pValues,
    fill = simPattern
  )
)

histPlot <- histPlot + geom_histogram(
  alpha = 0.5,
  position = "identity",
  binwidth  = 0.1
)

plot(histPlot)




