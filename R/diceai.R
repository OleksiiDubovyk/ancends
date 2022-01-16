diceai <- function(data){
  m <- as.data.frame(ifelse((abs(data)==0), 0, 1))
  if(ncol(data)>2){
    as.data.frame(outer(m, m, Vectorize(function(x, y){
      sum(ifelse(x+y==2, 1, 0))/min(sum(x), sum(y))
    })))
  }else{
    sum(ifelse(m[,1]+m[,2]==2, 1, 0))/min(sum(m[,1]), sum(m[,2]))
  }
}
