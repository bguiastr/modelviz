
prm_scale <- function(x,FUN){
  x <- as.numeric(x)
  if(toupper(FUN)=='CUBIC'){
    tmp <- (x)^(1/3)
  }else{
    tmp <- x/do.call(FUN,list(x,na.rm=TRUE))
  }
  tmp <- round(tmp,digits=3)
  return(tmp)
}
