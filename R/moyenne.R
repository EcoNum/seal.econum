#' Mean
#'
#' @param x a vector
#'
#' @return a number
#' @export
#' @importFrom stats na.omit
#' @import magrittr
#'
#' @examples
#' moyenne(c(1,3,NA))
moyenne <-function(x){
  x %>% na.omit() -> x
  sum(x)/length(x)
}
