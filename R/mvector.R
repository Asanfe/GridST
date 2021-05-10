mvector <- function(x, sd = FALSE){
                    #El siguiente código adapta los datos leídos OpenRepGrid a una estructura de matriz.
   if(class(x)[1] == "repgrid"){
      x <- adapt(x)
   }
                    #Se calcula la dimensión de los elementos de la matriz.
  d <- dim(x)[1]
                    #Se construye un vector de 1s apropiado para la fórmula del vector de medias.
  UNO <- c(rep(1,d))
                    #Aplicamos la fórmula del vector de medias
  result <- (1/d) * t(x) %*% UNO
  colnames(result) <- c("Media")
                    #Código para añadir la desviación típica en caso de que se pida en los argumentos.
   if(sd){
     S <- (diag(smatrix(x))^(1/2))
     result <- cbind(result,S)
     colnames(result) <- c("Media","SD")
   }
  return(result)
}
