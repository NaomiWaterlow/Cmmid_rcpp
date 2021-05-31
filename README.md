# Cmmid_rcpp
 SIR with MCMC for CMMID meeting

To run the model fit run the Rcpp_model.R script.


The other files are:
- Rcpp_model_functions.R : this contains functions used to fit the model.
- SIR_model.cpp : the SIR model equations in cpp and the structure used to run them through R
- compiler_function.R : R function used to compile the cpp code.
- SIRsample.RDS : the data.

Packages used are MASS_7.3-51.6, Rcpp_1.0.6, deSolve_1.28, data.table_1.13.4, ggplot2_3.3.2          
