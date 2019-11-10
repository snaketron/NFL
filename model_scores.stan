data {
  int<lower=0> N; // games in season
  int<lower=0> Nt; // teams
  real<lower=0> y [N];
  int x [N]; // team id
  int home[N];
}

parameters {
  vector <lower = 0> [Nt] score_mu;
  vector [Nt] score_home;
  // vector <lower = 0> [Nt] score_sd;
  real <lower = 0> score_sd;
}

model {
  for(i in 1:N) {
    y[i] ~ normal(score_mu[x[i]] + score_home[x[i]]*home[i], score_sd);
    // ;score_sd[x[i]]);
  }
  
  score_mu ~ normal(0, 5);
  score_home ~ normal(0, 5);
  score_sd ~ cauchy(0, 1);
}

