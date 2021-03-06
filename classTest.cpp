#include <Rcpp.h>

//define a class node_tuple to store information about nodes (i,j)
class node_tuple {
  int i; int j; char i_state; char j_state;
public:
  //element assignment
  void set_i (int,char);
  void set_j (int,char);
  void set_all (int,int,char,char);
  //element accessors
  int get_i() {return i;}
  int get_j() {return j;}
  char get_i_state() {return i_state;}
  char get_j_state() {return j_state;}
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
  //not defining constructor....could be an issue
  .method("set_i", &node_tuple::set_i,"sets node i attributes")
  .method("set_j", &node_tuple::set_j,"sets node j attributes")
  .method("set_all", &node_tuple::set_all,"sets node tuple attributes")
  .method("get_i", &node_tuple::get_i,"gets node i index")
  .method("get_j", &node_tuple::get_j,"gets node j index")
  .method("get_i_state", &node_tuple::get_i_state,"gets node i state")
  .method("get_j_state", &node_tuple::get_j_state,"gets node j state")
  ;
}

typedef std::vector<node_tuple> node_tuple_list;

// [[Rcpp::export]]
List make_tuples(){
  node_tuple_list out(2);
  node_tuple out_1;
  node_tuple out_2;
  out_1.set_all(5,3,'s','i');
  out_2.set_all(3,2,'s','i');
  out[0] = out_1;
  out[1] = out_2;
  bool hi = out[1].get_j_state()=='f'; //testing character comparisons
  Rcout << hi << std::endl;
  Rcout << out_1.get_i() << std::endl;
  return(List::create(Named("out")=out));
}


/***R
tmp <- make_tuples()
*/