#' Bikecourse function
#'
#' Findin association between a numeric variable x and a variable y with a boxplot (with significance letters) or a scatterplot (with regressionline) as output
#' @param x numeric variable x (written as data$x)
#' @param y variable y (written as data$y)
#' @param data name of the dataframe
#' @param threshold threshold for significance (default is 0.05)
#' @param xlab name of the x-axis of boxplot (default is x)
#' @param ylab name of the y-axis of boxplot (default is y)
#' @param yax location of significance letters on the y-axis
#' @param size size of the significance letters (default is 7)
#' @export
#' @example
#' bikecourse_function(Data$count, Data$season, data = Data, threshold = 0.05, xlab = "season", ylab = "count", yax = 900, size = 7 )

bikecourse_function <- function(x, y, data, threshold,
                                xlab, ylab,
                                yax, size){
  if(is.numeric(y)==FALSE){
    result <- Kruskal_function(x,y,data)
    if(result$p.value<=threshold){
      print("Association")
      return(dunn_function(x,y,threshold,xlab,ylab,yax,size))
    }
    else{
      print("No association")
    }
  }
  else{
    result <- Spearman_function(x,y)
    if(result$p.value<=treshold){
      print("Association")
      return(ggplot_function(x,y,xlab,ylab,data))
    }
    else{
      print("No association")
    }
  }
}
