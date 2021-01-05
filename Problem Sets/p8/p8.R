# 1a
library(Rcpp)

cppFunction("
  int mult(int x, int y){
    int result = x * y;
    return result;
  }")
mult(25L, 24L)

# 1b
cppFunction("
  double multiply(NumericVector x, NumericMatrix M){
    int sum = 0;
    for (int i=0;i< x.length();i++){
      if (x(i) < 1) {
        sum = sum + M(0, 0);
      }
      else if(x(i) <= M.nrow() && x(i) >= 1) {
        sum = sum + M(x(i)-1,x(i)-1);
      }
      else {
        sum = sum + M(M.nrow()-1,M.ncol()-1);
      }
    }
    return sum;
  }")
A = round(runif(6,min=0,max=10))
B = round(replicate(10,runif(10, min=0,max=100)))
multiply(A,B)

B[5,5] + 3*B[2,2] + B[9,9] + B[1,1]

# 1c -- FIX: Verify this works

cppFunction("
  NumericVector multiplyc(NumericVector x, NumericVector y, NumericMatrix M){
    NumericVector z(x.length());
    std::cout << z(2) << std::endl;
    for (int j=0; j <x.length(); j++) {
        for (int i=0;i < x.length(); i++) {
          z(j) = z(j) + M(x(i)-1,j) * M(y(i)-1,j);
        }
    }
    return z;
  }")
A = round(runif(6,min=1,max=10))
B = round(replicate(5,runif(10, min=0,max=100)))
C = round(runif(6,min=1,max=10))
multiplyc(A,C,B)

# 2a


R_file <- "C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/p8/odd.R"
cat("
    #' Determine in R if an integer is odd#'
    #' #' @param value An integer#' @return TRUE/FALSE of Whether the integer is odd
    #' #' @examples#' R_is_odd(10L)
    #' #' R_is_odd(11L)
    #' R_is_odd <- function(value = 0L){
    #' result <- ((value %% 2L) == 1L)
    #' return(result)}
    #' ", file = R_file)


cpp_file <- "C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/p8/odd.cc"
cat("
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
    ", file = cpp_file)
# 2b
library("Rcpp")
unlink("C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/p8/odder", recursive = TRUE)
Rcpp.package.skeleton(name = "odder",
                      path = "C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/p8",
                      code_files = R_file,
                      cpp_files = cpp_file,
                      force = TRUE,
                      environment = environment())

# 2c
path = "C:/Users/kaany/OneDrive/Desktop/Statistical Programming/Problem Sets/p8"
matrix(dir(paste0(path,"odder"), recursive = TRUE), ncol = 1)
  
unlink(paste0(path,"odder/Read-and-delete-me"))
unlink(paste0(path,"odder/src/rcpp_hello_world.cpp"))
unlink(paste0(path,"odder/man/rcpp_hello_world.Rd"))

cat(system("cat ~/Downloads/odder/DESCRIPTION", intern = TRUE), sep = "\n")


cat("Package: odderType: PackageTitle: Determine If Integers Are OddVersion: 1.0Date: 2020-10-01Author: Robert DaviesMaintainer: Robert Davies <robertwilliamdavies@gmail.com>Description: This package includes multiple functions to determine ifan integer is odd or not.License: GPL3Imports: Rcpp (>= 1.0.2)LinkingTo: Rcpp", file = "~/Downloads/odder/DESCRIPTION")file.copy("LICENSE.txt", "~/Downloads/odder/")