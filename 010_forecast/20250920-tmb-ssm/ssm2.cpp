// 二項分布のDGLM

#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // DATA //
  DATA_VECTOR(Y);  // 観測値

  // PARAMETER //
  PARAMETER_VECTOR(mu); // 状態
  PARAMETER(log_s_w);   // 過程誤差の標準偏差の対数
  PARAMETER(m0);        // 状態の初期値
  
  // PARAMETER TRANSFORMATION //
  Type s_w = exp(log_s_w);
  vector<Type> p = 1 / (1 + exp(-mu)); // muをロジスティック変換して、成功確率とみなす
  Type size = 1.0;

  // Main
  int T = Y.size();   // サンプルサイズ
  Type nll = 0.0;     // 負の対数尤度
  
  // 状態の初期値から、1時点目の状態を得る
  nll -= dnorm(mu(0), m0, s_w, true);
  
  // 状態方程式に従い、状態が遷移
  for (int t = 1; t < T; t++) {
    nll -= dnorm(mu(t), mu(t - 1), s_w, true);
  }
  
  // 観測方程式に従い、観測値が得られる
  for (int t = 0; t < T; t++) {
    nll -= dbinom(Y(t), size, p(t), true);
  }
  
  ADREPORT(p);
  
  return nll;
}
