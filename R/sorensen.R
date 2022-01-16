sorensen <- function(data){
  mutated <- as.data.frame(ifelse((abs(data)==0), 0, 1))
  if(ncol(data)>2){
    as.data.frame(outer(mutated, mutated, Vectorize(function(x, y){
      sum <- sum(ifelse(((x+y)<2), 0, 1))
      2*sum/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1))))
    })))
  }else{
    sum <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
    2*sum/((sum(ifelse(((mutated[,1])<1), 0, 1)))+(sum(ifelse(((mutated[,2])<1), 0, 1))))
  }
}
