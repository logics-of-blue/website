data {
  int T;                       // データ取得期間の長さ
  int user_num;                // ユーザー数
  matrix[T, user_num] y;       // 観測値
  matrix[T, user_num] ad_flag; // ユーザーごと広告フラグ
}

parameters {
  vector[T] x;        // 状態の推定値
  vector[user_num] r; // ユーザー毎のランダム効果
  real<lower=0> s_w;  // 過程誤差の標準偏差
  real<lower=0> s_v;  // 観測誤差の標準偏差
  real<lower=0> s_r;  // ランダム効果の標準偏差
  real b_ad;          // 広告による売り上げ増加効果
}

model {
  // 状態方程式に従い、状態が遷移する
  for(i in 2:T) {
    x[i] ~ normal(x[i-1], s_w);
  }
  
  // ランダム効果
  r ~ normal(0, s_r);
  
  // 観測方程式に従い、観測値が得られる
  for(i in 1:T) {
    for(j in 1:user_num) {
      y[i, j] ~ normal(x[i] + r[j] + ad_flag[i,j]*b_ad, s_v);
    }
  }
}
