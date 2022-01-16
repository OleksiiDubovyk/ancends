hills <- function(data, q = 0){
  p <- data/sum(data)
  if (q%%1!=0){
    stop("q must be an integer")
  }else if (q < 0){
    stop("q must be non-negative")
  }else if (q == 1){
    exp(-sum(p*log(p)))
  }else{
    sum(p^q)^(1/(1-q))
  }
}
