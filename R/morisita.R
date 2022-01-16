morisita <- function(data, log.transform = FALSE){
  if(log.transform==TRUE) data <- log(data+1)
  if(ncol(data)>2){
    as.data.frame(outer(data, data, Vectorize(function(x, y){
      2*(sum(x*y))/(((sum(x*(x-1)))/(sum(x)*(sum(x)-1))+(sum(y*(y-1)))/(sum(y)*(sum(y)-1)))*sum(x)*sum(y))
    })))
  }else{
    2*(sum(data[,1]*data[,2]))/(((sum(data[,1]*(data[,1]-1)))/(sum(data[,1])*(sum(data[,1])-1))+(sum(data[,2]*(data[,2]-1)))/(sum(data[,2])*(sum(data[,2])-1)))*sum(data[,1])*sum(data[,2]))
  }
}
