prm_scale <- function(x, FUN = function(x) { x^(1/3) } ){
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  tmp <- do.call(FUN, list(x))
  tmp <- round(tmp,digits=3)
  return(tmp)
}
