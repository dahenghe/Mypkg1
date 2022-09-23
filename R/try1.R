#' Estimate and visualize correlation between two numerical variables
#' @export
#' @param x numerical vector of the first variable
#' @param y numerical vector of the second variable
MyCorr <- function(x, y){
  return(cor.test(x, y))
}

MyCorr.plot <- function(x, y){
  plot(x=x, y=y,
       xlab = "X", ylab = "Y",
       main = paste("Spearman Corr = ", signif(cor.test(x, y)$estimate, 3), "\n", "Spearman P = ", signif(cor.test(x, y)$p.value, 3), sep = ""))
  abline(lm(y ~ x), lwd = 2, col="red")
}
