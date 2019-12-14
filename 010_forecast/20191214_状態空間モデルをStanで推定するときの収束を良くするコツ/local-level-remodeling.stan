data {
  int T;         // データ取得期間の長さ
  vector[T] y;   // 観測値
}

parameters {
  real<lower=0> s_w;  // 過程誤差の標準偏差
  real<lower=0> s_v;  // 観測誤差の標準偏差
  
  vector[T] mu_err;   // 水準成分の増減量

}

transformed parameters {
  // transformed parametersで実際の水準成分を得る
  
  vector[T] mu;        // 水準成分の推定値

  // 1時点目
  mu[1] = mu_err[1];

  // 2時点目以降
  for (t in 2:T) {
    // 標準偏差1の正規乱数に、s_wをかけて実際の過程誤差としている
    mu[t] = mu[t-1] + s_w * mu_err[t];
  }
}


model {
  // mu_err[1]は、無情報事前分布を指定
  
  // mu_err[2]以降は、標準偏差1の正規乱数を得る
  mu_err[2:T] ~ normal(0,1);

  // 観測方程式
  y ~ normal(mu, s_v);
}
