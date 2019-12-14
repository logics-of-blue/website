data {
  int T;        // データ取得期間の長さ
  vector[T] y;  // 観測値
}

parameters {
  vector[T] mu;       // 水準+ドリフト成分の推定値
  vector[T] delta;    // ドリフト成分の推定値
  real<lower=0> s_w;  // 水準成分の変動の大きさを表す標準偏差
  real<lower=0> s_z;  // ドリフト成分の変動の大きさを表す標準偏差
  real<lower=0> s_v;  // 観測誤差の標準偏差
}

model {
  // 状態方程式に従い、状態が遷移する
  for(t in 2:T) {
    mu[t] ~ normal(mu[t-1] + delta[t-1], s_w);
    delta[t] ~ normal(delta[t-1], s_z);
  }
  
  // 観測方程式に従い、観測値が得られる
  for(t in 1:T) {
    y[t] ~ normal(mu[t], s_v);
  }

}
