#include <Rcpp.h>

//TESTING THE NODE_TUPLE CLASS
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

//NOT WORKING CURRENTLY:: FIND OUT WHY
/*
 * its not working because in node_tuple_list out(2); there is no default arguments being called
 * to the constructor. eg; its trying to make a vector of length 2 where each element is the node_tuple
 * object but it doesnt know what to do with it. so we need some default args.
 */
//node_tuple_list
typedef std::vector<node_tuple> node_tuple_list;

// [[Rcpp::export]]
List make_tuples(){
  node_tuple_list out(2);
  node_tuple out_1 (2,2,'i','r');
  node_tuple out_2 (2,1,'r','s');
  out[0] = out_1;
  out[1] = out_2;
  // bool hi = out[1].get_j_state()=='f'; //testing character comparisons
  // Rcout << hi << std::endl;
  // Rcout << out_1.get_i() << std::endl;
  return(List::create(Named("out")=out));
}

// [[Rcpp::export]]
node_tuple make_a_tuple(){
  node_tuple out (2,2,'r','r');
  return(out);
}

// [[Rcpp::export]]
node_tuple make_a_tuple_args(int i,int j,char i_state,char j_state){
  node_tuple out (i,j,i_state,j_state);
  return(out);
}


/***R
tuple <- node_tuple$new(3,4,"s","i")
tuple$get_i()
tuple$get_i_state()
tuple$set_all(2,2,"f","f")
tuple$get_i()

tuple1 <- make_a_tuple()
tuple2 <- make_a_tuple_args(4,2,"s","r")
tuple2$get_j_state()

list_tuples <- make_tuples()

#generate the network medium the simulation will be run on
  library(igraph)
  erdos <- erdos.renyi.game(n=100,p=0.025,directed=TRUE,loops=FALSE)
  erdos_edge <- as_edgelist(erdos)
  erdos_edge <- erdos_edge[order(erdos_edge[,1]),]
*/