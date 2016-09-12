#include <Rcpp.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>

/*
 * node_tuple class stores information for two nodes (i,j):
 * node_tuple_list is a std::vector that will store the node tuples and function as an edgelist
 * that can be complemented with additional information (currently node indices and state)
 */
class node_tuple {
public:
  //constructor
  node_tuple() {} //empty constructor for initializing the continer vector
  node_tuple(int i, int j, char i_state, char j_state): i(i), j(j), i_state(i_state), j_state(j_state) {}
  //element assignment
  void set_i (int,char);
  void set_j (int,char);
  void set_all (int,int,char,char);
  //element accessors
  int get_i() {return i;}
  int get_j() {return j;}
  char get_i_state() {return i_state;}
  char get_j_state() {return j_state;}
private:
  int i; 
  int j; 
  char i_state; 
  char j_state;
};

//define element assignment methods
void node_tuple::set_i(int set_i, char set_i_state){
  i = set_i;
  i_state = set_i_state;
}
void node_tuple::set_j(int set_j, char set_j_state){
  j = set_j;
  j_state = set_j_state;
}
void node_tuple::set_all(int set_i, int set_j, char set_i_state, char set_j_state){
  i = set_i;
  i_state = set_i_state;
  j = set_j;
  j_state = set_j_state;
}

using namespace Rcpp;

//expose the node_tuple class to allow export to R
RCPP_EXPOSED_CLASS(node_tuple)
  RCPP_MODULE(mod_node_tuple){
    class_<node_tuple>("node_tuple")
    .constructor<int,int,char,char>("initialize node_tuple object")
    .method("set_i", &node_tuple::set_i,"sets node i attributes")
    .method("set_j", &node_tuple::set_j,"sets node j attributes")
    .method("set_all", &node_tuple::set_all,"sets node tuple attributes")
    .method("get_i", &node_tuple::get_i,"gets node i index")
    .method("get_j", &node_tuple::get_j,"gets node j index")
    .method("get_i_state", &node_tuple::get_i_state,"gets node i state")
    .method("get_j_state", &node_tuple::get_j_state,"gets node j state")
    ;
  }

//node_tuple_list functions as an edgelist (contactList in PLoS paper)
typedef std::vector<node_tuple> node_tuple_list;

/*
 * generate a node_tuple_list at t=0 from igraph edgelist input
 */





/*
 * update_si updates m_SI; the takes a node_tuple_list at time t and returns a vector of 
 * indicies where either the i_state or j_state of the node tuple at that index has one infected. 
 */

/***R
#generate the network medium the simulation will be run on
library(igraph)
  erdos <- erdos.renyi.game(n=100,p=0.025,directed=TRUE,loops=FALSE)
  erdos_edge <- as_edgelist(erdos)
  erdos_edge <- erdos_edge[order(erdos_edge[,1]),]


*/