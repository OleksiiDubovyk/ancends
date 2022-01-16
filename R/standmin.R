standmin <- function(x, to.max = FALSE){
  if(to.max==FALSE) return(x+(abs(min(x))))
  if(to.max==TRUE){
    x <- x+(abs(min(x)))
    return(x/max(x))
  }
}
