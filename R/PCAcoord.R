PCAcoord <- function(x, n=3){
  if(class(x)[1] >= "repgrid"){
    x <- adapt(x)
  }
  jdim <- dim(x)[2]
  if(n <= 0){
    cat("el valor minimo para n es 1")
  }
  else{
  if(n <= jdim){
  RC <- x %*% eigen(smatrix(x))$vectors[,1:n]
  colnames(RC) <- paste("Coord", 1:n)
  return(RC)}
  else{
    cat("El valor de n (número de componentes principales) excede la dimensión de la matriz de análisis")
  }
  }
}
