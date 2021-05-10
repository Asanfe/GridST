VG <- function(x, sd=FALSE){
  result <- det(smatrix(x))
  if(sd){
    result <- sqrt(result)
  }
  return(result)
}
