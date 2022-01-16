hpie <- function(data){
  if (length(data[data > 0]) < 2){
    0
  }else{
    N <- sum(data)
    (N/(N-1))*(1-sum((data/N)^2))
  }
}
