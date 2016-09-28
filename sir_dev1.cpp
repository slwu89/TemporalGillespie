#include <Rcpp.h>
using namespace Rcpp;

/*
 * Gillespie SSA to run SIR model on static network (simulation of time-homogeneous CTMC on static network medium)
 * direct port of R code. need to optimize.
 */


typedef std::vector<char> node_states; //vector of strings to record states {S,I,R}
typedef std::vector<node_states> node_states_output; //vector of node_states to output history

// [[Rcpp::export]]
List sir_homogeneous(int n_nodes, NumericMatrix edge, int root, double beta, double mu, int t_end, bool info=true){

  node_states_output output; //output vector

  node_states x(n_nodes); //node states
  std::fill(x.begin(),x.end(),'s'); //set node states to I
  x[root] = 'i'; //set state of root node to I

  std::vector<int> m_I; //vector of infected nodes
  m_I.reserve(n_nodes);
  m_I.push_back(root);
  int N_I = 1; //number of infected nodes
  int N_R = 0; //number of recovered nodes
  double Mu = mu; //cumulative recovery rate
  double tau = R::rexp(1.0); //draw tau ~ exp(1)

  //run through time-steps
  for(int t=0; t<t_end; t++){

    //print simulation diagnostics
    if(info){
      Rcout << "current t: " << t << ", number infected: " << N_I << ", number recovered: " << N_R << ", number susceptible: " << n_nodes-N_I-N_R << std::endl;
    }

    //update list of possible S to I transitions
    std::vector<int> m_SI; //S nodes in contact with I nodes
    m_SI.reserve(n_nodes);
    for(int k=0; k<edge.nrow(); k++){
      int i = edge(k,0) - 1; //fix for erdos_edge starting at 1, NOT SURE
      int j = edge(k,1) - 1;
      if(x.at(i) == 's' && x.at(j) == 'i'){
        m_SI.push_back(i);
      }
      if(x.at(i) == 'i' && x.at(j) == 's'){
        m_SI.push_back(j);
      }
    } //end for
    
    int M_SI = m_SI.size();
    double Beta = beta * M_SI; //cumulative infection rate
    double Lambda = Mu + Beta; //cumulative transition rate
    
    //check if a transition takes place
    if(tau >= Lambda){ //no transition
      tau -= Lambda;
    } else { //at least one transition
      double xi = 1.0; //remaining fraction of time-step
      while(tau < xi*Lambda){
        xi -= tau/Lambda; //fraction of time-step left after transition
        double z = R::runif(0.0,Lambda-2E-16);
        if(z < Beta){ //S to I transition
          int m;
          if(m_SI.size()==1){ //draw m at random from m_SI
            m = m_SI[0];
          } else {
            int index = floor(R::runif(0,m_SI.size()));
            m = m_SI[index];
          }
          x[m] = 'i';
          m_I.push_back(m);
          N_I += 1;
          Mu += mu;
        } else { //I to R transition
          int m;
          if(m_I.size()==1){ //draw m at random from m_I
            m = m_I[0];
          } else {
            int index = floor(R::runif(0,m_I.size()));
            m = m_I[index];
          }
          x[m] = 'r';
          std::remove(m_I.begin(),m_I.end(),m); //remove m from m_I
          N_I -= 1;
          N_R += 1;
          Mu -= mu;
        } //end if

        //update list of possible S to I transitions
        m_SI.clear(); //S nodes in contact with I nodes
        for(int k=0; k<edge.nrow(); k++){
          int i = edge(k,0) - 1;
          int j = edge(k,1) - 1;
          if(x.at(i) == 's' && x.at(j) == 'i'){
            m_SI.push_back(i);
          } else if(x.at(i) == 'i' && x.at(j) == 's'){
            m_SI.push_back(j);
          } else {
            continue;
          }
        } //end for

        output.push_back(x); //return output

        M_SI = m_SI.size();
        Beta = beta*M_SI; //cumulative infection rate
        Lambda = Mu + Beta; //cumulative transition rate
        tau = R::rexp(1.0); //draw new tau


      } //end while
    } //end if

    if(N_I==0){
      return(List::create(Named("output")=output));
    }

  } //end for

  //return output
  return(List::create(Named("output")=output));
}

/***R
set.seed(1)
library(igraph)
n_nodes <- 200
erdos <- erdos.renyi.game(n=n_nodes,p=0.025,directed=FALSE,loops=FALSE)
erdos_edge <- as_edgelist(erdos)
erdos_edge <- erdos_edge[order(erdos_edge[,1]),]

beta <- 1.15/5 # R0 / infectious duration
mu <- 1/5 # 1 / infectious duration

sir_out <- sir_homogeneous(n_nodes=n_nodes,edge=erdos_edge,root=5,beta=beta,mu=mu,t_end=50,info=TRUE)
*/