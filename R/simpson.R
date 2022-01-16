simpson <- function(data, mode = "Biased"){
  if(mode == "Biased"){
    if(is.null(ncol(data))==F){
      return(apply(data, 2, function(data) (1-sum((data/sum(data))^2))))
    }
    if(is.null(ncol(data))==T){
      return(1-sum((data/sum(data))^2))
    }
  }
  if(mode == "Unbiased"){
    if(is.null(ncol(data))==F){
      return(apply(data, 2, function(data) (1-(sum(data*(data-1)))/(sum(data)*(sum(data)-1)))))
    }
    if(is.null(ncol(data))==T){
      return(1-(sum(data*(data-1)))/(sum(data)*(sum(data)-1)))
    }
  }
}
