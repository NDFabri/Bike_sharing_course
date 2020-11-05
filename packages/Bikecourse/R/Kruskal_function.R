#' Kruskal wallis
#'
#' Kruskal wallis test between a numeric variable x and a factor variable y
#' @param x numeric variable x (should be written as data$x)
#' @param y variable y (should be written as data$y)
#' @param data name of the dataframe
#' @export
#' @example
#' Kruskal_function(Data$count, Data$season, data=Data)

Kruskal_function <- function(x,y,data){
  kruskal.test(x ~ y, data=data)
}
