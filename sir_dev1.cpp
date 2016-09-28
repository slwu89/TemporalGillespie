#include <Rcpp.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>

using namespace Rcpp;

/*
 * Gillespie SSA to run SIR model on static network (simulation of time-homogeneous CTMC on static network medium)
 * direct port of R code. need to optimize.
 */

typedef std::vector<std::string> node_states; //vector of strings to record states {S,I,R}
typedef std::vector<node_states> node_states_output; //vector of node_states to output history

// List sir_homogeneous(int n_nodes, NumericMatrix edge, int root, double beta, double mu, int t_end, bool info=true){
//   
//   node_states_output output; //output vector
//   
//   node_states x(n_nodes); //node states
//   std::fill(x.begin(),x.end(),'s'); //set node states to I
//   x[root] = 'i'; //set state of root node to I
//   
//   return(List::create(Named("x")=x));
// }

// [[Rcpp::export]]
List sir_homogeneous(int n_nodes, int root){
  
  node_states_output output; //output vector
  
  node_states x(n_nodes); //node states
  std::fill(x.begin(),x.end(),'s'); //set node states to I
  x[root] = 'i'; //set state of root node to I
  
  return(List::create(Named("x")=x));
}