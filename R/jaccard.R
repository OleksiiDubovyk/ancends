jaccard <- function(data){
  m <- as.data.frame(ifelse((abs(data)==0), 0, 1))
  if(ncol(data)>2){
    as.data.frame(outer(m, m, Vectorize(function(x, y){
      sum(ifelse(((x+y)<2), 0, 1))/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1)))-sum(ifelse(((x+y)<2), 0, 1)))
    })))
  }else{
    sum(ifelse(((m[,1]+m[,2])<2), 0, 1))/((sum(ifelse(((m[,1])<1), 0, 1)))+(sum(ifelse(((m[,2])<1), 0, 1)))-sum(ifelse(((m[,1]+m[,2])<2), 0, 1)))
  }
}
