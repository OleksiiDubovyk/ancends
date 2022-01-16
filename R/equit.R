equit <- function(data, log.base){
  if(is.null(ncol(data))==F){
    if(missing(log.base)){
      return(apply(data, 2, function(x) ((-sum((x[x>0]/sum(x))*log(x[x>0]/sum(x)))))/log(length(x[x>0])) ))
    }else{
      return(apply(data, 2, function(x) ((-sum((x[x>0]/sum(x))*log(x[x>0]/sum(x)))))/log(length(x[x>0]), log.base) ))
    }
  }
  if(is.null(ncol(data))==T){
    if(missing(log.base)){
      return((-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data))))/log(length(data[data>0])))
    }else{
      return((-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data))))/log(length(data[data>0]), log.base))
    }
  }
}
