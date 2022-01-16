ochiai <- function(data){
  mutated <- as.data.frame(ifelse((abs(data)==0), 0, 1))
  if(ncol(data)>2){
    as.data.frame(outer(mutated, mutated, Vectorize(function(x, y){
      a <- sum(ifelse((x==0), 0, 1))
      b <- sum(ifelse((y==0), 0, 1))
      c <- sum(ifelse(((x+y)<2), 0, 1))
      c/sqrt(a*b)
    })))
  }else{
    a <- sum(ifelse((mutated[,1]==0), 0, 1))
    b <- sum(ifelse((mutated[,2]==0), 0, 1))
    c <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
    c/sqrt(a*b)
  }
}
