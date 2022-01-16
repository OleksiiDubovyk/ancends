margalef <- function(data, log.base){
  if(is.null(ncol(data))==F){
    if(missing(log.base)){
      return(apply(data, 2, function(data) (length(data[data>0])-1)/log(sum(data)) ))
    }else{
      return(apply(data, 2, function(data) (length(data[data>0])-1)/log(sum(data), log.base) ))
    }
  }
  if(is.null(ncol(data))==T){
    if(missing(log.base)){
      return((length(data[data>0])-1)/log(sum(data)))
    }else{
      return((length(data[data>0])-1)/log(sum(data), log.base))
    }
  }
}
