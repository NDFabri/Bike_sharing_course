#' Scatterplot with regressionline
#'
#' Scatterplot of a numeric variable x and a numeric variable y with a linear regressionline
#' @param x numeric variable x (should be written as data$x)
#' @param y variable y (should be written as data$y)
#' @param data name of the dataframe
#' @export
#' @example
#' ggplot_function(Data$count, Data$temp, xlab="temp", ylab="count", data=Data)

ggplot_function <- function(x,y,xlab,ylab,data){
  ggplot(data, aes(x=y, y=x))+
    geom_point() + theme_bw() +
    geom_smooth(method="lm") +
    xlab(xlab) + ylab(ylab)
}

