code_event_times_missing <- nimbleCode({
  
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
    
    # + phi[Week_SpUnit[i]]

  } 
  
  for (i in 1:N){
    a[i] <- Day_from[i] - 0.5
    b[i] <- Day_to[i] + 0.5
    Day_event[i] ~ dunif(a[i], b[i])
    Day_event_rounded[i] <- round(Day_event[i])
    
    cTuesday[i] <- rel_Tuesday[Day_event_rounded[i]]
    Tuesday[i] <- cTuesday[i]
    cWednesday[i] <- rel_Wednesday[Day_event_rounded[i]]
    Wednesday[i] <- cWednesday[i]
    cThursday[i] <- rel_Thursday[Day_event_rounded[i]]
    Thursday[i] <- cThursday[i]
    cFriday[i] <- rel_Friday[Day_event_rounded[i]]
    Friday[i] <- cFriday[i]
    cSaturday[i] <- rel_Saturday[Day_event_rounded[i]]
    Saturday[i] <- cSaturday[i]
    cSunday[i] <- rel_Sunday[Day_event_rounded[i]]
    Sunday[i] <- cSunday[i]
   
    cWeek[i] <- rel_Week[Day_event_rounded[i]]
    Week[i] <- cWeek[i]
    
    cWeek_SpUnit[i] <- 105*(SpUnit[i]-1)+Week[i]-SpUnit[i]+1
    Week_SpUnit[i] <- cWeek_SpUnit[i]
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
  
  # Space-time effect
  # for (i in 1:N_INT){
  #   phi[i] ~ dnorm(0,tau.phi)
  # }
  # sigma2.phi ~ dgamma(1, 0.01)
  # tau.phi <- 1/sigma2.phi
  
})
