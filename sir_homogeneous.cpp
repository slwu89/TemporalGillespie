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
  void set_i_state (char);
  void set_j (int,char);
  void set_j_state (char);
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
void node_tuple::set_i_state(char set_i_state){
  i_state = set_i_state;
}
void node_tuple::set_j_state(char set_j_state){
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
    .method("set_i_state", &node_tuple::set_i_state,"sets node i state")
    .method("set_j_state", &node_tuple::set_j_state,"sets node j state")
    .method("get_i", &node_tuple::get_i,"gets node i index")
    .method("get_j", &node_tuple::get_j,"gets node j index")
    .method("get_i_state", &node_tuple::get_i_state,"gets node i state")
    .method("get_j_state", &node_tuple::get_j_state,"gets node j state")
    ;
  }

//node_tuple_list functions as an edgelist (contactList in PLoS paper)
typedef std::vector<node_tuple> node_tuple_list;

/*
 * generate a node_tuple_list at t=0 from igraph edgelist input, corresponding to contactList
 * in PLoS paper.
 */
//TEST FUNCTION EXPORT SEXP LIST; when put in C export the std::vector node_tuple_list
//right now everyone susceptible
// [[Rcpp::export]]
List init_node_tuple_list(NumericMatrix edge, int root){
  
  //allocate contactList
  int n_edge = edge.nrow();
  node_tuple_list contactList(n_edge);
  
  for(int i=0; i<n_edge; i++){
    
    //generate tuple i
    int i_index = edge(i,0);
    int j_index = edge(i,1);
    node_tuple tuple_i (i_index,j_index,'s','s');
    //check for root node
    if(i_index == root){
      tuple_i.set_i_state('i');
    }
    if(j_index == root){
      tuple_i.set_j_state('i');
    }
    contactList[i] = tuple_i;
    
  }
  
  return(List::create(Named("out")=contactList));
}

//real function
node_tuple_list init_contactList(NumericMatrix edge, int root){
  //allocate contactList
  int n_edge = edge.nrow();
  node_tuple_list contactList(n_edge);
  
  for(int i=0; i<n_edge; i++){
    
    //generate tuple i
    int i_index = edge(i,0);
    int j_index = edge(i,1);
    node_tuple tuple_i (i_index,j_index,'s','s');
    //check for root node
    if(i_index == root){
      tuple_i.set_i_state('i');
    }
    if(j_index == root){
      tuple_i.set_j_state('i');
    }
    contactList[i] = tuple_i;
    
  }
  
  return(contactList);
}


//function to return the number of unique elements of an edgelist
//basically equivalent to length(unique(c(matrix[,1],matrix[,2])))
// [[Rcpp::export]]
List num_unique_rcpp(NumericMatrix input){
  NumericVector elements;
  for(int i=0;i<input.nrow();i++){
    elements.push_back(input(i,0));
    elements.push_back(input(i,1));
  }
  std::sort(elements.begin(), elements.end());
  std::unique(elements.begin(), elements.end());
  int n_unique = 0;
  int i = 0;
  while(elements(i) != elements(i+1)){
    n_unique += 1;
    i++;
  }
  elements.erase(n_unique,elements.length());
  return(List::create(Named("n_unique")=n_unique,Named("elements")=elements));
}

int num_unique(NumericMatrix input){
  NumericVector elements;
  for(int i=0;i<input.nrow();i++){
    elements.push_back(input(i,0));
    elements.push_back(input(i,1));
  }
  std::sort(elements.begin(), elements.end());
  std::unique(elements.begin(), elements.end());
  int n_unique = 0;
  int i = 0;
  while(elements(i) != elements(i+1)){
    n_unique += 1;
    i++;
  }
  return(n_unique);
}

/*
 * Need to go through the contactList and create list of susceptible nodes in contact
 * with infectious nodes
 * in actuality this will take as input the contactList but we need to input some stuffs
 * in order to test the function alone in Rcpp
 * this creates a list of susceptible nodes in contact with infectious nodes
 */
// [[Rcpp::export]]
List generate_si_list(NumericMatrix edge, int root){
  
  node_tuple_list contactList = init_contactList(edge, root); //real input is contactList
  node_tuple_list si_list;
  
  for(int i=0; i<contactList.size(); i++){
    
    //if neither infectious go to next iteration of loop (more efficient?)
    if(contactList[i].get_i_state() == 's' && contactList[i].get_j_state() == 's'){
      continue;
    } else {
      si_list.push_back(contactList[i]);
    }
    
  } //end loop
  
  return(List::create(Named("si_list")=si_list));
}


/***R
#generate the network medium the simulation will be run on
library(igraph)
  erdos <- erdos.renyi.game(n=100,p=0.04,directed=TRUE,loops=FALSE)
  erdos_edge <- as_edgelist(erdos)
  erdos_edge <- erdos_edge[order(erdos_edge[,1]),]
  
contactList <- init_node_tuple_list(erdos_edge,5)
t(sapply(contactList$out,function(x){c(x$get_i(),x$get_j(),x$get_i_state(),x$get_j_state())}))

num_unique_rcpp(erdos_edge)
*/


/*
 * sir_homogeneous will run the temporal Gillespie algorithm for the SIR model on a network defined
 * by an edgelist, which is the edge argument to the function. root is an integer: it tells us
 * which node is the initial infected
 */
// // [[Rcpp::export]]
// void sir_homogeneous(NumericMatrix edge, int root, double mu, int t_end){
//   
//   //number of nodes
//   int n_nodes = num_unique(edge);
//   
//   //initialize vector of node states
//   // std::vector<char> node_states(n_nodes);
//   // for(int i=0;i<n_nodes;i++){
//   //   node_states.push_back('s');
//   // }
//   
//   //initialize vector of node states
//   CharacterVector node_states;
//   for(int i=0;i<n_nodes;i++){
//     node_states.push_back('s');
//   }
//   Rcout << node_states << std::endl;//DEBUGGING
//   //set state of root node to i
//   node_states(root-1) = 'i';
//   
//   //number of nodes in each state
//   int n_inf = 1;
//   int n_rec = 0;
//   int n_sus = n_nodes - n_inf;
//   
//   //draw initial tau
//   double tau = R::rexp(1.0);
//   
//   //initialize contactList
//   node_tuple_list contactList = init_contactList(edge,root);
//   
//   //main simulation loop
//   for(int t=0; t<t_end; t++){
//     
//   }
//   
// }