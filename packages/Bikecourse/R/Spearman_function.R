#' Spearman rank
#'
#' Spearman rank test between a numeric variable x and a numeric variable y
#' @param x numeric variable x (should be written as data$x)
#' @param y variable y (should be written as data$y)
#' @export
#' @example
#' Spearman_function(Data$count, Data$temp)

Spearman_function <- function(x,y){
  cor.test(x, y, method="spearman")
}







