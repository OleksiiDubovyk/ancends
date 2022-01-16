bergerparker <- function(data){
  if(is.null(ncol(data))==F){
    return(apply(data, 2, function(data) max(data)/sum(data) ))
  }
  if(is.null(ncol(data))==T){
    return(max(data)/sum(data))
  }
}
