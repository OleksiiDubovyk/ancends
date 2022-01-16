braycurtis <- function(data, as.per.cent = FALSE, log.transform = FALSE, log.base){
  if(as.per.cent==TRUE) m <- as.data.frame(apply(data, 2, function(x) 100*abs(x)/sum(abs(x))))
  if(as.per.cent==FALSE) m <- as.data.frame(apply(data, 2, abs))
  if(log.transform==TRUE){
    if(missing(log.base)){
      m <- log(m+1)
    }else{
      m <- log(m+1, log.base)
    }
  }
  if(log.transform==FALSE){
    m <- m
  }
  if(ncol(data)>2){
    as.data.frame(outer(m, m, Vectorize(function(x, y){
      1-sum(abs(x-y))/sum(x+y)
    })))
  }else{
    1-sum(abs(m[,1]-m[,2]))/sum(m[,1]+m[,2])
  }
}
