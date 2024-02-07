data {
  int<lower=1> K; // number of outcomes
  int<lower=1> T; // number of time points following treatment
  int<lower=1> J; // number of patient-level predictors
  int<lower=1> N; // number of observations
  int<lower=1> P; // number of patients
  int<lower=1> S; // number of sites
  int<lower=1,upper=P> p[N]; // patient id
  int<lower=1,upper=S> s[N]; // site id
  matrix[P,J] x; // patient-level predictors
  matrix[N,T] time; // treatment time indicator
  int<lower=-1,upper=1> y[N,K]; // outcome matrix
}

parameters {
  real alpha;
  vector[K] alpha_k;
  real<lower=0> tau_k;
  vector[S] alpha_s;
  real<lower=0> tau_s;
  vector[P] alpha_p;
  real<lower=0> tau_p;
  vector[J] beta;
  vector[T] theta;
  vector[T] theta_k[K];
  vector<lower=0>[2] tau_t;
}

transformed parameters {
  vector[K] mu[N];

  for (k in 1:K) {
    for (i in 1:N) {
        // baseline
        mu[i, k] = alpha + alpha_k[k] + alpha_s[s[i]] + alpha_p[p[i]] + x[p[i]] * beta +
          time[i] * (theta + theta_k[k]);
    }
  }
}

model {
  // priors
  alpha ~ normal(0, 10);
  alpha_k ~ normal(0, tau_k);
  alpha_s ~ normal(0, tau_s);
  alpha_p ~ normal(0, tau_p);
  tau_k ~ student_t(4, 0, 1);
  tau_s ~ student_t(4, 0, 1);
  tau_p ~ student_t(4, 0, 1);
  beta ~ normal(0, 2.5);
  theta ~ normal(0, 2.5);
  for (t in 1:T) {
    for (k in 1:K) {
      theta_k[k, t] ~ normal(0, tau_t[t]);
    }
  }

  // likelihood
  for (i in 1:N) {
    for (k in 1:K) {
      if (y[i, k] < 0) {
        increment_log_prob(log_mix(inv_logit(mu[i, k]), bernoulli_logit_log(1, mu[i, k]), bernoulli_logit_log(0, mu[i, k])));
      } else {
        increment_log_prob(bernoulli_logit_log(y[i, k], mu[i, k]));
      }
    }
  }
}

generated quantities {
   vector[K] y_pred[N];
   for (i in 1:N) {
    for (k in 1:K) {
      y_pred[i, k] = bernoulli_logit_rng(mu[i, k]);
    }
   }
}
