#' Dunn graph
#'
#' Boxplot between a numeric variable x and a factor variable y with difference between groups based on dunn test
#' @param x numeric variable x (written as data$x)
#' @param y variable y (written as data$y)
#' @param threshold threshold for significance (default is 0.05)
#' @param xlab name of the x-axis of boxplot (default is x)
#' @param ylab name of the y-axis of boxplot (default is y)
#' @param yax location of significance letters on the y-axis
#' @param size size of the significance letters (default is 7)
#' @export
#' @example
#' dunn_function(Data$count, Data$season, threshold = 0.05, xlab ="season", ylab = "count", yax = 900, size = 7)

dunn_function <- function(x, y, threshold=0.05,
                          xlab="x", ylab="y", yax, size=7){
  test <- dunn.test::dunn.test(x, y, method = "sidak", table=FALSE, list=FALSE)
  frame <- data.frame(test$P.adjusted, test$comparisons)
  pvals <- test$P.adjusted
  A <- NROW(frame)
  dst <- matrix(NA, ((1+sqrt(1-(-8*A)))/2),((1+sqrt(1-(-8*A)))/2))
  dst[lower.tri(dst)] <- pvals
  dst <- as.dist(dst)
  dst <- as.matrix(dst, upper=TRUE, lower=TRUE)
  diag(dst) <- 1
  LET <- multcompLetters(dst,
                         compare="<",
                         threshold=threshold,
                         Letters=letters,
                         reversed = FALSE)
  T <- (1+sqrt(1-(-8*A)))/2
  G <- data.frame(c(1:T))
  G$let <- LET$Letters
  ggplot(data = Data, mapping = aes(x = y, y = x, group=y)) +
    geom_boxplot(alpha = 0) + theme_bw() +
    xlab(xlab)+ ylab(ylab) +
    annotate("text", x=G$c.1.T., y=yax, label=G$let, size=size)
}

