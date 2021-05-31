#include <Rcpp.h>
#include <R.h>
using namespace Rcpp;

// global parms variable that can be accessed in other functions
Rcpp::List global_parms;

//Function to return the parms list passed to deSolve as a SEXP object
SEXP attribute_hidden get_deSolve_gparms_Rcpp()
{
  static SEXP(*fun)() = NULL;
  if (fun == NULL)
    fun = (SEXP(*)()) R_GetCCallable("deSolve","get_deSolve_gparms");
  return fun();
}

// Defining the functiosn used
extern "C" {
  void derivatives (int *neq, double *t, double *y, double *ydot,
                    double *yout, int *ip);
  void initmod(void(* odeparms)(int *, double *));
}

void initmod (void(* odeparms)(int *, double *))
{
  //Get the parms argument passed to deSolve as SEXP object
  SEXP sparms = get_deSolve_gparms_Rcpp();
  try {
    //Parse parameters passed to deSolve as Rcpp::List
    Rcpp::List parms = Rcpp::clone(Rcpp::as<Rcpp::List>(sparms));
    // garbage collection
    global_parms = parms;
  } catch(std::exception& __ex__){
    forward_exception_to_r(__ex__);
  } catch(...){
    ::Rf_error( "c++ exception (unknown reason)" );
  }
}

void derivatives (int *neq, double *t, double *y, double *ydot,
                  double *yout, int *ip){
  
  float beta = global_parms["beta"];
  float gamma = global_parms["gamma"];
  int N = global_parms["N"];
  
// Susceptible
  ydot[0] = - (beta*y[0]*y[1])/N;
// Infected
  ydot[1] = + (beta*y[0]*y[1])/N - gamma*y[1];
// Recovered
  ydot[2] = gamma*y[1];
// Cumulative infections  
  ydot[3] = (beta*y[0]*y[1])/N;
  

}

