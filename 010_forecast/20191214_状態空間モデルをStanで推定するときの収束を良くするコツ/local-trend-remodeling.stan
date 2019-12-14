data {
  int T;               // データ取得期間の長さ
  vector[T] y;         // 観測値
}

parameters {
  real<lower=0> s_w;   // 水準成分の変動の大きさを表す標準偏差
  real<lower=0> s_z;   // ドリフト成分の変動の大きさを表す標準偏差
  real<lower=0> s_v;   // 観測誤差の標準偏差
  
  vector[T] mu_err;    // 水準成分の増減量
  vector[T] delta_err; // ドリフト成分の増減量
}

transformed parameters {
  // transformed parametersで実際の状態を得る

  vector[T] mu;        // 水準+ドリフト成分の推定値
  vector[T] delta;     // ドリフト成分の推定値
  
  // 1時点目
  mu[1] = mu_err[1];
  delta[1] = delta_err[1];
  
  // 2時点目以降
  for (t in 2:T) {
    mu[t] = mu[t-1] + delta[t-1] + s_w * mu_err[t];
    delta[t] = delta[t-1] + s_z * delta_err[t];
  }
}

model {
  // 標準偏差1の正規乱数を得る
  mu_err[2:T] ~ normal(0,1);
  delta_err[2:T] ~ normal(0,1);

  // 観測方程式
  y ~ normal(mu, s_v);
}
