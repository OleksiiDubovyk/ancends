percentage <- function(data, per.cent=FALSE){
  if(ncol(data)>1){
    if(per.cent==TRUE) return(as.data.frame(apply(data, 2, function(x) 100*abs(x)/sum(abs(x)))))
    if(per.cent==FALSE) return(as.data.frame(apply(data, 2, function(x) abs(x)/sum(abs(x)))))
  }
  if(ncol(data)==1){
    if(per.cent==TRUE) return(100*abs(data)/sum(abs(data)))
    if(per.cent==FALSE) return(abs(data)/sum(abs(data)))
  }
}
