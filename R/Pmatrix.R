#' @title Matriz de correlaciones parciales.
#' @description Función que nos va a dar como resultado las correlaciones parciales
#' entre las diferentes variables de la matriz de datos
#' @param x matriz de datos que se va a analizar.
#' @return Matriz con las correlaciones parciales entre las diferentes variables de la matriz de datos
#' @export Pmatrix
#' @examples
Pmatrix <- function(x){

#Escribimos el título del resultado.
  cat("\n#################################")
  cat("\nMatriz de correlaciones parciales")
  cat("\n#################################\n\n")

#Comprobamos que tengamos la librería de OpenRepGrid.
  require(OpenRepGrid)

#Adaptamos el formato de OpenRepGrid a una matriz estandar.
  if(class(x)[1] == "repgrid"){
    x <- adapt(x)
  }

#Construimos una matriz - I de dimensiones adecuadas al número de constructos.
  d <- dim(x)[2]
  NUNO <- -diag(d)

#Construimos la matriz diagonal de la matriz de precisión.
  D <- d * d
  DS <- matrix(rep(0,D), ncol= d)
  diag(DS) <- diag(prematrix(x))

#aplicamos la fórmula de la matriz de de correlaciones parciales.
  result <- NUNO %*% solve(DS)^(1/2) %*% prematrix(x) %*% solve(DS)^(1/2)

#Corregimos la diagonal de la matriz y le damos nombres a filas y columnas.
  diag(result) <- c(rep(1,d))
  rownames(result) <- colnames(x)
  colnames(result) <- colnames(x)
  return(result)
}

