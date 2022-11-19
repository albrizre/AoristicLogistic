code_event_times_complete_cases <- nimbleCode({
  
  alpha ~ dnorm(0,0.001)
  beta_tuesday ~ dnorm(0,0.001)
  beta_wednesday ~ dnorm(0,0.001)
  beta_thursday ~ dnorm(0,0.001)
  beta_friday ~ dnorm(0,0.001)
  beta_saturday ~ dnorm(0,0.001)
  beta_sunday ~ dnorm(0,0.001)
  
  for (i in 1:N) {
    Response[i] ~ dbern(pi[i])
    logit(pi[i]) <- alpha + beta_tuesday*Tuesday[i] + beta_wednesday*Wednesday[i] + beta_thursday*Thursday[i] +
                  beta_friday*Friday[i] + beta_saturday*Saturday[i] + beta_sunday*Sunday[i] + 
                  delta[Week[i]] + epsilon[Week[i]] + u[SpUnit[i]] + v[SpUnit[i]] 
  } 
  
  # Unstructured week effect
  for (i in 1:N_W){
    epsilon[i] ~ dnorm(0,tau.epsilon)
  }
  sigma2.epsilon ~ dgamma(1,0.5)
  tau.epsilon <- 1/sigma2.epsilon

  # RW2 prior on the effect of week
  delta[1:N_W] ~ dcar_normal(adj[1:N_W_adj], weights[1:N_W_adj], num[1:N_W], tau.delta, zero_mean = 1)
  sigma2.delta ~ dgamma(1,0.5)
  tau.delta <- 1/sigma2.delta

  # Unstructured spatial effect
  for (i in 1:G) {
    v[i] ~ dnorm(0,tau.v)
  }
  sigma2.v ~ dgamma(1, 0.01)
  tau.v <- 1/sigma2.v

  # Structured spatial effect
  u[1:G] ~ dcar_normal(adj_grid[1:N_grid_w], weights_grid[1:N_grid_w], num_grid[1:G], tau.u, zero_mean = 1) 
  sigma2.u ~ dgamma(1, 0.01)
  tau.u <- 1/sigma2.u

})
