renkonen <- function(data, per.cent=FALSE){
  m <- as.data.frame(apply(data, 2, function(x) abs(x)/sum(abs(x))))
  if(ncol(data)>2){
    result <- as.data.frame(outer(m, m, Vectorize(function(x, y) sum(apply(data.frame(x, y), 1, min)))))
  }else{
    result <- sum(apply(m, 1, min))
  }
  if(per.cent==TRUE) return(result*100)
  if(per.cent==FALSE) return(result)
}
