log_sorensen <- function(data, as.per.cent=FALSE, result.as.per.cent=FALSE){
  if(as.per.cent==TRUE) m <- as.data.frame(apply(data, 2, function(x) 100*abs(x)/sum(abs(x))))
  if(as.per.cent==FALSE) m <- as.data.frame(apply(data, 2, abs))
  if(ncol(data)>2){
    result <- as.data.frame(outer(m, m, Vectorize(
      function(x, y){
        2*sum(apply(data.frame(x, y), 1, function(x) ifelse((min(x)==0|min(x)==0&max(x)==0), 0, log(min(x)+1)/log(max(x)+1))))/((length(x[x>0]))+(length(y[y>0])))
      })))
  }else{
    a <- sum(ifelse((m[,1]==0), 0, 1))
    b <- sum(ifelse((m[,2]==0), 0, 1))
    result <- 2*sum(apply(m, 1, function(x) ifelse((min(x)==0|min(x)==0&max(x)==0), 0, log(min(x)+1)/log(max(x)+1))))/(a+b)
  }
  if(result.as.per.cent==TRUE) return(result*100)
  if(result.as.per.cent==FALSE) return(result)
}
