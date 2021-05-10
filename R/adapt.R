adapt <- function(x){
  require(OpenRepGrid)
  result <- getRatingLayer(x)
  colnames(result) <- getElementNames(x)
  rownames(result) <- getConstructNames(x)[,2]
  return(t(result))
}
