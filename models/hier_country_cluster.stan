
data {
  int<lower=1> N;  // Number of observations
  int<lower=1> NC; // number of countries
  int<lower=1> NCl; // number of clusters
  array[N] int<lower=1, upper=NC> C;  // country vector
  array[N] int<lower=1, upper=NCl> Cl; // cluster vector
  array[N] int<lower=0, upper=1> M; // married
  array[N] int<lower=0, upper=1> E; // not educated
  array[N] int<lower=0, upper=1> P; // poor
  array[N] int<lower=0, upper=1> R; // rural area
  array[N] real Pop; // log population density
  array[N] real U; // absolute UTCI index
  array[N] real DU; // deviation of UTCI index
  array[N] int<lower=0, upper=1> V;  // violence
  array[N] real<lower=0> W; // weights
  int prior_only; // return only priors
}

parameters {
  real a;
  real <lower=0> a_country_std;
  real <lower=0> a_cluster_std;
  // real <lower=0> b_utci_country_std;
  // real <lower=0> b_utci_cluster_std;
  array[NC] real a_country_offset;
  array[NCl] real a_cluster_offset;
  // array[N] real b_utci_country_offset;
  // array[N] real b_utci_cluster_offset;
  real <lower=0> a_country_hyp;
  real <lower=0> a_cluster_hyp;
  real b_married;
  real b_not_educated;
  real b_poor;
  real b_rural;
  real b_utci;
}

transformed parameters {
  array[N] real a_country;
  array[N] real a_cluster;
  // array[N] real b_utci_country;
  // array[N] real b_utci_cluster;
  for (c in 1:NC) {
    a_country[c] = a_country_offset[c] * a_country_std;
    // b_utci_country[c] = b_utci_country[c] * b_utci_country_std;
  }
  for (cl in 1:NCl) {
    a_cluster[cl] = a_cluster_offset[cl] * a_cluster_std;
    // b_utci_cluster[cl] = b_utci_cluster[cl] * b_utci_cluster_std;
  }
  real lprior = 0;
  lprior += student_t_lpdf(a | 6, -4, 1);
  lprior += exponential_lpdf(a_country_std|0.2);
  lprior += exponential_lpdf(a_cluster_std|0.2);
  lprior += normal_lpdf(b_married|0, 0.2);
  lprior += normal_lpdf(b_not_educated|0, 0.2);
  lprior += normal_lpdf(b_poor|0, 0.2);
  lprior += normal_lpdf(b_rural|0, 0.2);
  lprior += normal_lpdf(b_utci|0, 0.2);
  vector[N] p_viol;
  for (i in 1:N) {
    p_viol[i] = (
      a
      + a_country[C[i]]
      + a_cluster[Cl[i]]
      + b_married * M[i]
      + b_not_educated * E[i]
      + b_poor * P[i]
      + b_rural * R[i]
      // + b_pop * Pop[i]
      + b_utci * DU[i]
      // + b_utci_country[C[i]] * DU[i]
      // + b_utci_cluster[Cl[i]] * DU[i]
    );
  };
}

model {
  // a ~ student_t(6, -4, 1);
  // a_country_offset ~ exponential(a_country_hyp);
  // a_cluster_offset ~ exponential(a_cluster_hyp);
  // a_country_hyp ~ exponential(0.2);
  // a_cluster_hyp ~ exponential(0.2);
  // a_country_std ~ normal(0, 1);
  // a_cluster_std ~ normal(0, 1);
  // b_married ~ normal(0, 0.2);
  // b_not_educated ~ normal(0, 0.2);
  // b_poor ~ normal(0, 0.2);
  // b_rural ~ normal(0, 0.2);
  // b_utci ~ normal(0, 0.2);
  if (!prior_only) {
    for (i in 1:N) {
      target += W[i] * bernoulli_logit_lpmf(V[i] | p_viol[i]);
    }
  }
  target += lprior;
  for (i in 1:N) {
    target += std_normal_lpdf(a_country_offset[C[i]]);
    target += std_normal_lpdf(a_cluster_offset[Cl[i]]);
  }
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    real p_viol_hat = inv_logit(
      a
      + a_country[C[i]]
      + a_cluster[Cl[i]]
      + b_married * M[i]
      + b_not_educated * E[i]
      + b_poor * P[i]
      + b_rural * R[i]
      // + b_pop * Pop[i]
      + b_utci * DU[i]
      // + b_utci_country[C[i]] * DU[i]
      // + b_utci_cluster[Cl[i]] * DU[i]
    ); 
    log_lik[i] = bernoulli_lpmf(V[i]|p_viol_hat);
  };
}

