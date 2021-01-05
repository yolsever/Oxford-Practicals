
    #include <Rcpp.h>using namespace Rcpp;
    //' Determine if integer is odd or not
    //'
    //' @param value    Integer value
    //'
    //' @return boolean of whether value is odd
    //'
    //' @export// [[Rcpp::export]]
    bool cpp_is_odd(int value = 0){
      bool result = (value % 2) == 1;
      return result;}
    