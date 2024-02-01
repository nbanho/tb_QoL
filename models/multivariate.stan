data {
  int<lower=1> K; // number of outcomes
  int<lower=1> J; // number of predictors
  int<lower=1> N; // number of observations
  vector[J] x[N]; // predictor vector
  vector[K] y[N]; // outcome vector
  int y_missing[N,K]; // missing outcome indicator
  int<lower=0> M; // number of missing values
}

parameters {
  vector[M] y_imp;
  matrix[K, J] beta;
  cholesky_factor_corr[K] L_Omega;
  vector<lower=0>[K] L_sigma;
}

transformed parameters {
  vector[K] mu[N];
  matrix[K, K] L_Sigma;
  vector[K] y_tilde[N];
  
  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);

  for (k in 1:K) {
    for (i in 1:N) {
        mu[i, k] = beta[k] * x[i];
        if (y_missing[i, k] > 0) {
            y_tilde[i, k] = y_imp[y_missing[i, k]];
        } else if (y_missing[i, k] < 1) {
            y_tilde[i, k] = y[i, k];
        }
    }
  }
}

model {
  to_vector(beta) ~ normal(0, 5.21);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0, 2.5);

  y_tilde ~ multi_normal_cholesky(mu, L_Sigma);
}

generated quantities {
   vector[K] y_pred[N] = multi_normal_cholesky_rng(mu, L_Sigma);
}
