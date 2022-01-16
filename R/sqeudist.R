sqeudist <- function(data, log.transform = FALSE){
  m <- as.data.frame(apply(data, 2, function(x) x/sum(x)))
  if(log.transform==TRUE) m <- log(m+1)
  if(ncol(data)>2){
    as.data.frame(outer(m, m, Vectorize(function(x, y){
      1-sum((x-y)^2)
    })))
  }else{
    1-sum((m[,1]-m[,2])^2)
  }
}
