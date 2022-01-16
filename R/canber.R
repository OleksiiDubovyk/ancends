canber <- function(data, zeros, log.transform = FALSE){
  if(log.transform==TRUE) data <- log(data+1)
  if(missing(zeros)){
    result <- as.data.frame(outer(data, data, Vectorize(function(x, y){
      mutated <- data.frame(c(ifelse((abs(x)==0), 0, 1)), c(ifelse((abs(y)==0), 0, 1)))
      a <- sum(mutated[,1])
      b <- sum(mutated[,2])
      c <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
      1-((sum(abs((ifelse(x==0, 0.01, x))-(ifelse(y==0, 0.01, y)))/((ifelse(x==0, 0.01, x))+(ifelse(y==0, 0.01, y)))))*(1/(a+b-c)))
    })))
  }else{
    result <- as.data.frame(outer(data, data, Vectorize(function(x, y){
      mutated <- data.frame(c(ifelse((abs(x)==0), 0, 1)), c(ifelse((abs(y)==0), 0, 1)))
      a <- sum(mutated[,1])
      b <- sum(mutated[,2])
      c <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
      1-((sum(abs((ifelse(x==0, zeros, x))-(ifelse(y==0, zeros, y)))/((ifelse(x==0, zeros, x))+(ifelse(y==0, zeros, y)))))*(1/(a+b-c)))
    })))
  }
  if(ncol(data)==2){
    return(result[1,2])
  }else{
    return(result)
  }
}
