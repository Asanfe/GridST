Rmatrix <- function(x){

#Adaptamos el formato de OpenRepGrid a una matriz estandar.
  if(inherits(x,"repgrid")){
  x <- adapt(x)
  }

#Construimos la matriz D(S)^-1/2.
  d <- dim(x)[2]
  DS <- matrix(c(rep(0,d*d)), ncol = d)
  diag(DS) <- diag(smatrix(x))
  DS1 <- solve(sqrt(DS))

#Aplicamos la formula de la matriz de correlaciones.
  result <- DS1 %*% smatrix(x) %*% DS1
  rownames(result) <- colnames(x)
  colnames(result) <- colnames(x)

  return(result)
}
