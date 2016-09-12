#include <Rcpp.h>

// class Test {
// public:
//   Test(int x): x_(x) {}
//   int getValue() { return x_; }
//   void addValue(int y) { x_ += y; }
//   void merge(const Test& rhs) { x_ += rhs.x_; }
// private:
//   int x_;
// };
// 
// class Test1 {
// public:
//   Test1(int x, char y): x_(x), y_(y) {}
//   int get_x() { return x_; }
//   char get_y() { return y_; }
// private:
//   int x_;
//   char y_;
// };

//TESTING THE NODE_TUPLE CLASS
class node_tuple {
public:
  node_tuple(int i, int j, char i_state, char j_state): i(i), j(j), i_state(i_state), j_state(j_state) {}
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

using namespace Rcpp;

// RCPP_EXPOSED_CLASS(Test)
//   RCPP_MODULE(mod_test) {
//     
//     class_<Test>("Test")
//     
//     .constructor<int>("sets initial value")
//     
//     .method("getValue", &Test::getValue, "Returns the value")
//     .method("addValue", &Test::addValue, "Adds a value")
//     .method("merge", &Test::merge, "Merges another Test into this object")
//     ;
//   }
// 
// RCPP_EXPOSED_CLASS(Test1)
//   RCPP_MODULE(mod_test1){
//     class_<Test1>("Test1")
//     .constructor<int,char>("set up test1")
//     .method("get_x", &Test1::get_x,"returns x")
//     .method("get_y", &Test1::get_y,"returns y")
//     ;
//   }

RCPP_EXPOSED_CLASS(node_tuple)
  RCPP_MODULE(mod_node_tuple){
    class_<node_tuple>("node_tuple")
    .constructor<int,int,char,char>("initialize node_tuple object")
    .method("get_i", &node_tuple::get_i,"gets node i index")
    .method("get_j", &node_tuple::get_j,"gets node j index")
    .method("get_i_state", &node_tuple::get_i_state,"gets node i state")
    .method("get_j_state", &node_tuple::get_j_state,"gets node j state")
    ;
  }

/***R
tuple <- node_tuple$new(3,4,"s","i")
tuple$get_i()
tuple$get_i_state()
*/