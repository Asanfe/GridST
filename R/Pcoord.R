Pcoord <- function(x){
  Q <- QDmatrix(x)
  cA <- c(eigen(Q)$values[1], eigen(Q)$values[2])
  A <- matrix(rep(0,4), ncol=2)
  diag(A) <- cA
  V <- cbind(eigen(Q)$vectors[,1],eigen(Q)$vectors[,2])
  PC <- V %*% sqrt(A)
  rownames(PC) <- getElementNames(x)
  return(PC)
  }
