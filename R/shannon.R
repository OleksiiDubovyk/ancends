shannon <- function(data, mode = "Biased", log.base, std = FALSE){
  if(mode == "Biased"){
    if(is.null(ncol(data))==F){
      if(missing(log.base)){
        H <- apply(data, 2, function(x) -sum((x[x>0]/sum(x))*log(x[x>0]/sum(x))))
      }else{
        H <- apply(data, 2, function(x) -sum((x[x>0]/sum(x))*log((x[x>0]/sum(x)), log.base)))
      }
    }
    if(is.null(ncol(data))==T){
      if(missing(log.base)){
        H <- -sum((data[data>0]/sum(data))*log(data[data>0]/sum(data)))
      }else{
        H <- -sum((data[data>0]/sum(data))*log((data[data>0]/sum(data)), log.base))
      }
    }
  }
  if(mode == "Unbiased"){
    if(is.null(ncol(data))==F){
      if(missing(log.base)){
        H <- apply(data, 2, function(x) (-sum((x[x>0]/sum(x))*log(x[x>0]/sum(x))))-((length(x[x>0])-1)/(2*sum(x))))
      }else{
        H <- apply(data, 2, function(x) (-sum((x[x>0]/sum(x))*log((x[x>0]/sum(x)), log.base)))-((length(x[x>0])-1)/(2*sum(x))))
      }
    }
    if(is.null(ncol(data))==T){
      if(missing(log.base)){
        H <- (-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data))))-((length(data[data>0])-1)/(2*sum(data)))
      }else{
        H <- (-sum((data[data>0]/sum(data))*log((data[data>0]/sum(data)), log.base)))-((length(data[data>0])-1)/(2*sum(data)))
      }
    }
  }
  if (std == F){
    return(H)
  }else{
    if (length(H) > 1){
      S <- apply(data, 2, function(x) sum(x > 0))
    }else{
      S <- sum(data > 0)
    }
    if (missing(log.base)){
      return(H/log(S))
    }else{
      return(H/log(S, base = log.base))
    }
  }
}
