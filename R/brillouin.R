brillouin <- function(data, log.base){
  if(is.null(ncol(data))==F){
    if(missing(log.base)){
      return(apply(data, 2, function(data) ((log(factorial(sum(data))))-sum(log(factorial(data))))/sum(data)) )
    }else{
      return(apply(data, 2, function(data) ((log((factorial(sum(data))), log.base))-sum(log((factorial(data)), log.base)))/sum(data)))
    }
  }
  if(is.null(ncol(data))==T){
    if(missing(log.base)){
      return( ((log(factorial(sum(data))))-sum(log(factorial(data))))/sum(data) )
    }else{
      return( ((log((factorial(sum(data))), log.base))-sum(log((factorial(data)), log.base)))/sum(data) )
    }
  }
}
