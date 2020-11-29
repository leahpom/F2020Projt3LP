#' Test Function
#'
#' Takes a vector and returns its square. Also gives a statement as to what the purpose is
#'
#' @param x
#'
#' @return a vector of squared values in the form of a vector
#' @return a string of characters
#'
#' @export
#'
#' @examples
#' x=1:30; testf(x)
testf <- function(x){
  x.sq <- x^2
  char <- "This is a test to see that I have correctly set up my package and built a basic workable function"
  test.list <- list("x squared" = x.sq, char)
  return(test.list)
}
