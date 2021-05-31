# Rcpp model

# Load packages
library(ggplot2)
library(data.table)
library(deSolve)
library(Rcpp)
library(MASS)
library(tmvtnorm)

# format the data 
input_data <- readRDS("SIRsample.rds")
data_to_fit <- data.table(matrix(0,nrow=433, ncol = 3))
colnames(data_to_fit) <- c("timestep", "Onset", "Recover")
data_to_fit$timestep <- c(1:433)
data_to_fit[timestep %in% input_data$times, Onset := unlist(input_data[,"onsets"])]
data_to_fit[timestep %in% input_data$times, Recover := unlist(input_data[,"recoveries"])]

# compile the model
compileModel("SIR_model.cpp", "build")
# load model
dyn.load("build/SIR_model.so")
# load functions
source("Rcpp_model_functions.R")
#start conditions
init_theta <- list(beta = 5, gamma = 5, N = pop_size)
prop_sd <- c(0.005,0.005)
covmat <- matrix(0,nrow = 2, ncol = 2)
diag(covmat) <- prop_sd

#Run
trace <- run_mcmc(iterations =20000, 
                  init_theta <- init_theta, 
                  covmat = covmat, 
                  lower = lower,
                  upper = upper)

# acceptance rate
nrow(unique(trace))/nrow(trace)
# format to plot
trace$step <- 1:nrow(trace)
colnames(trace) <- c("ll", "beta", "gamma", "step")
trace$beta <- 1/ trace$beta
trace$gamma <- 1/trace$gamma
trace_m <- reshape2::melt(trace, id.vars = "step")
# plot trace
TRACE <- ggplot(trace_m, aes( x= step, y = value)) + 
  facet_wrap(.~variable, scales = "free_y", ncol = 1)+
  geom_line()
TRACE

trace_burn <- trace_m[which(trace_m$step > 5000),]
# plot density
DENSITY <- ggplot(trace_burn) + 
  geom_density(aes(x = value)) + 
  facet_wrap(.~variable, scales = "free", ncol = 1)
DENSITY  
