cmatrix <- function(x){
  if(class(x)[1] >= "repgrid"){
    x <- adapt(x)
  }
  d <- dim(x)[1]
  UNO <- c(rep(1,d))
  x - UNO %*% t(mvector(x))
}
