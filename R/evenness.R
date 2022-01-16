evenness <- function(data, mode = "Biased", log.base){
  e <- 2.71828182845904523536028747
  if(mode == "Biased"){
    if(is.null(ncol(data))==F){
      if(missing(log.base)){
        return(apply(data, 2, function(x) (e^(-sum((x[x>0]/sum(x))*log(x[x>0]/sum(x)))))/length(x[x>0])  ))
      }else{
        return(apply(data, 2, function(x) (e^(-sum((x[x>0]/sum(x))*log((x[x>0]/sum(x)), log.base))))/length(x[x>0])  ))
      }
    }
    if(is.null(ncol(data))==T){
      if(missing(log.base)){
        return((e^(-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data)))))/length(data[data>0]) )
      }else{
        return((e^(-sum((data[data>0]/sum(data))*log((data[data>0]/sum(data)), log.base))))/length(data[data>0])  )
      }
    }
  }
  if(mode == "Unbiased"){
    if(is.null(ncol(data))==F){
      if(missing(log.base)){
        return(apply(data, 2, function(x) (e^((-sum((x[x>0]/sum(x))*log(x[x>0]/sum(x))))-((length(x[x>0])-1)/(2*sum(x)))))/length(x[x>0])  ))
      }else{
        return(apply(data, 2, function(x) (e^((-sum((x[x>0]/sum(x))*log((x[x>0]/sum(x)), log.base)))-((length(x[x>0])-1)/(2*sum(x)))))/length(x[x>0])  ))
      }
    }
    if(is.null(ncol(data))==T){
      if(missing(log.base)){
        return((e^((-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data))))-((length(data[data>0])-1)/(2*sum(data)))))/length(data[data>0])  )
      }else{
        return((e^((-sum((data[data>0]/sum(data))*log((data[data>0]/sum(data)), log.base)))-((length(data[data>0])-1)/(2*sum(data)))))/length(data[data>0])  )
      }
    }
  }
}

s.evenness <- function(data){
  e <- 2.71828182845904523536028747
  if(is.null(ncol(data))==F){
    return(apply(data, 2, function(x) (e^(-sum((x[x>0]/sum(x))*log(x[x>0]/sum(x)))))/length(x[x>0])))
  }
  if(is.null(ncol(data))==T){
    return((e^(-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data)))))/length(data[data>0]))
  }
}
