statsElements2 <- function(x, index=TRUE, trim=20)
{
  if (!inherits(x, "repgrid")) 							      # check if x is repgrid object
    stop("Object x must be of class 'repgrid'")
  s <- getRatingLayer(x)
  res <- describe(s)                              # psych function
  enames <- getElementNames2(x, index=index, trim=trim)
  ne <- getNoOfElements(x)
  if (length(unique(enames)) != ne){
    stop("please chose a longer value for 'trim' or set 'index' to TRUE",
         "as the current value produces indentical rownames")
  }
  rownames(res) <- enames
  class(res) <- c("statsElements", "data.frame")
  return(res)
}
