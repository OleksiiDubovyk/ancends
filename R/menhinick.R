menhinick <- function(data){
  if(is.null(ncol(data))==F){
    return(apply(data, 2, function(data) length(data[data>0])/sqrt(sum(data))))
  }
  if(is.null(ncol(data))==T){
    return(length(data[data>0])/sqrt(sum(data)))
  }
}
