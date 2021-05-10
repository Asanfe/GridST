Dmatrix <- function(x){
  d <- dim(x)[2]
  UNO <- c(rep(1,d))
  D <- diag(Qmatrix(x)) %*% t(UNO) + UNO %*% t(diag(Qmatrix(x))) - 2 * Qmatrix(x)
  rownames(D) <- getElementNames(x)
  return(D)
}
