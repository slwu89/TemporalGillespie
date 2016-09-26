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
 * Gillespie algorithm to simulate SIR model on static network medium.
 * it will simulate a single trajectory of a time-homogeneous CTMC process.
 * THIS IS PURELY FOR DEVELOPMENT
 */

//grab number of unique nodes from edge list
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

//given an edge list and a root infected, generate a node_tuple_list object
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

//given a contactList (a node_tuple_list object), generate another node_tuple_list that stores
//all of the tuples where a SI transition is possible
node_tuple_list generate_si_list(node_tuple_list contactList){
  
  node_tuple_list si_list;
  
  for(int i=0; i<contactList.size(); i++){
    
    //if neither infectious go to next iteration of loop;
    //dont want to store any combination of S or R people
    if(contactList[i].get_i_state() != 'i' && contactList[i].get_j_state() != 'i'){
      continue;
    } else {
      si_list.push_back(contactList[i]);
    }
    
  } //end loop
  
  return(si_list);
}

/*
 * note root uses 0 subsetting
 * beta: base infection rate
 * mu: base recovery rate
 */
void sir_homogeneous(NumericMatrix edge, int root, double beta, double mu, int t_end){

  //number of nodes
  int n_nodes = num_unique(edge);

  //initialize vector of node states
  CharacterVector node_states;
  for(int i=0;i<n_nodes;i++){
    node_states.push_back('s');
  }
  Rcout << node_states << std::endl;//DEBUGGING
  //set state of root node to i
  node_states(root) = 'i';

  //number of nodes in each state
  int n_inf = 1;
  int n_rec = 0;
  int n_sus = n_nodes - n_inf;

  //draw initial tau
  double tau = R::rexp(1.0);

  //initialize contactList
  node_tuple_list contactList = init_contactList(edge,root);

  //main simulation loop
  double time = 0.0; //time
  int iter = 0; //iterator
  double beta_cum; //cumulative infection rate
  double lambda_cum; //cumulative transition rate

  while(time <= t_end){
    
    //generate list of possible SI transitions
    node_tuple_list si_list = generate_si_list(contactList);
    
    int si_n = si_list.size(); //number of SI tuples
    beta_cum = si_n * beta; //cumulative infection rate
    lambda_cum = beta_cum + mu; //cumulative transition rate
    
    // Check if transition takes place during time-step:
    if(tau >= lambda_cum){ //no transition takes place
      tau -= lambda_cum;
    } else { //at least one transition took place
      
    }
    
    iter++; //update iterator
  }

}