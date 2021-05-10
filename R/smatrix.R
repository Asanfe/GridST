smatrix <- function(x, c = FALSE){
                           #El siguiente código adapta los datos leídos OpenRepGrid a una estructura de matriz.
  if(class(x)[1] == "repgrid"){
    x <- adapt(x)
  }
                           #Se calcula la dimensión de los elementos de la matriz.
  d <- dim(x)[1]
                           #Se aplica la fórmula correspondiente para el cálculo de matriz de covarianzas
                           #en función de si se quiere corregida o no.
  if(c){
    result <- (1/(d-1)) * t(cmatrix(x)) %*% cmatrix(x)
    }
  else{
    result <- (1/d) * t(cmatrix(x)) %*% cmatrix(x)
  }
  return(result)
}
