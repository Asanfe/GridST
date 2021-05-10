Qmatrix <- function(x){
  Q <- cmatrix(x) %*% t(cmatrix(x))
  return(Q)
}

QDmatrix <- function(x){
  d <- dim(x)[2]
  I <- diag(d)
  UNO <- c(rep(1,d))
  P <- I - (1/d) * UNO %*% t(UNO)
  Q <- -(1/2) * P %*% Dmatrix(x) %*% P
  return(Q)

}
