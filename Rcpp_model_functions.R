#Rcpp_model functions


run_mcmc <- function(iterations = 1000,
                     init_theta, 
                     covmat, 
                     lower, 
                     upper){
  # specifics
  pop_size <- 10000
  init_state <- c(S = pop_size-1, I = 1, R = 0, C = 0)
  times = c(1:433)
  # inital run
  theta_old <- init_theta
  outall <- run_model(init_state, times, theta_old)
  ll_old <- calc_ll(outall, data_to_fit)
  total_trace <- data.frame(matrix(ncol = 3, nrow = iterations))
  
  
  for (i in 1:iterations){
    
    theta_propose <- mvrnorm(n =1, mu = as.numeric(theta_old[1:2]),
                              Sigma = covmat)
    
    theta_new <- list(beta =  theta_propose[1], 
                      gamma = theta_propose[2], 
                      N = pop_size)
    
    outall <- run_model(init_state, times, theta_new)
    ll_new <- calc_ll(outall, data_to_fit)
    
    ll_acceptance <- ll_new - ll_old
    #Not needed as symmetircal
    # ll_acceptance <- ll_acceptance + dtmvnorm(x = unlist(theta_old[1:2]), 
    #                                           mean = unlist(theta_old[1:2]), 
    #                                           sigma = covmat,
    #                                           lower = lower, 
    #                                           upper = upper, 
    #                                           log = TRUE)
    # 
    # ll_acceptance <- ll_acceptance - dtmvnorm(x = unlist(theta_new[1:2]), 
    #                                           mean = unlist(theta_old[1:2]), 
    #                                           sigma = covmat ,
    #                                           lower = lower, 
    #                                           upper = upper, 
    #                                           log = TRUE)
    
    if (is.accepted <- (log(runif(1)) < ll_acceptance)) {
      theta_old <- theta_new
      ll_old <- ll_new
    }
    total_trace[i,] <- c(ll_old, theta_old[1:2])
  }
  return(total_trace)
}


# Run Model
run_model <- function(init_state, times, parameters){

  parameters[[1]] <- 1/parameters[[1]]
  parameters[[2]] <- 1/parameters[[2]]
  
  outall <- data.table(ode(y = init_state, 
                           t = times, 
                           initfunc = "initmod",
                           dllname = "SIR_model",
                           func = "derivatives", 
                           parms = parameters, 
                           method = "rk4"))
  
  # onset incidence / recoveries
  outall[, incidence := (C - shift(C, 1L, type = "lag"))]
  outall[, recovered := (R - shift(R, 1L, type = "lag"))]
  outall[1,c("incidence", "recovered")] <- 0
  
  return(outall)
  
}

calc_ll <- function(outall, data_to_fit){
  
  data_to_fit[, I_ll := dpois(x = Onset, lambda = outall[["incidence"]], 
                              log = T)]
  data_to_fit[, R_ll := dpois(x = Recover, lambda = outall[["recovered"]], 
                              log = T)]
  
  ll <- sum(data_to_fit[,I_ll]) + sum(data_to_fit[,R_ll])
  return(ll)
}
