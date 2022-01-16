csf <- function(community, factors, method = "sorensen", transposed = FALSE, shorten.result = TRUE){
  if(transposed == FALSE){
    cenosis <- as.data.frame(t(community))
    factors <- as.data.frame(factors)
  }
  if(transposed == TRUE){
    cenosis <- as.data.frame(community)
    factors <- as.data.frame(t(factors))
  }
  if(method == "sorensen"){
    Similarity <- as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      x = ifelse((x==0), 0, 1)
      y = ifelse((y==0), 0, 1)
      sum <- sum(ifelse(((x+y)<2), 0, 1))
      2*sum/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1))))
    })))
  }
  if(method == "jaccard"){
    Similarity <- as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      x = ifelse((x==0), 0, 1)
      y = ifelse((y==0), 0, 1)
      sum(ifelse(((x+y)<2), 0, 1))/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1)))-sum(ifelse(((x+y)<2), 0, 1)))
    })))
  }
  if(method == "renkonen"){
    cenosis <- as.data.frame(apply(cenosis, 2, function(x) abs(x)/sum(abs(x))))
    Similarity <- as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      sum(apply(data.frame(x, y), 1, min))
    })))
  }
  if(method == "log.sorensen"){
    Similarity <- as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      2*sum(apply(data.frame(x, y), 1, function(x) ifelse((min(x)==0|min(x)==0&max(x)==0), 0, log(min(x)+1)/log(max(x)+1))))/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1))))
    })))
  }
  if(method == "braycurtis"){
    Similarity <- as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      m <- data.frame(x, y)
      1-sum(abs(m[,1]-m[,2]))/sum(m[,1]+m[,2])
    })))
  }
  if(method == "braycurtis2"){
    Similarity <- (as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      m <- data.frame(x, y)
      1-sum(abs(m[,1]-m[,2]))/sum(m[,1]+m[,2])
    }))))^2
  }
  if(method == "canber"){
    Similarity <- as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      mutated <- data.frame(c(ifelse((abs(x)==0), 0, 1)), c(ifelse((abs(y)==0), 0, 1)))
      a <- sum(mutated[,1])
      b <- sum(mutated[,2])
      c <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
      1-((sum(abs((ifelse(x==0, 0.01, x))-(ifelse(y==0, 0.01, y)))/((ifelse(x==0, 0.01, x))+(ifelse(y==0, 0.01, y)))))*(1/(a+b-c)))
    })))
  }
  if(method == "sqeudist"){
    Similarity <- as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      m <- data.frame(x, y)
      m = apply(m, 2, function(x) x/sum(x))
      1-sum((m[,1]-m[,2])^2)
    })))
  }
  if(method == "morisita"){
    Similarity <- as.vector(outer(cenosis, cenosis, Vectorize(function(x, y){
      2*(sum(x*y))/(((sum(x*(x-1)))/(sum(x)*(sum(x)-1))+(sum(y*(y-1)))/(sum(y)*(sum(y)-1)))*sum(x)*sum(y))
    })))
  }
  fcompare <- apply(factors, 2, function(x) as.vector(outer(as.numeric(x), as.numeric(x), '-')))
  result <- as.data.frame(cbind(Similarity, fcompare))
  if(shorten.result == TRUE){
    result = abs(result[apply(result[2:ncol(result)], 1, function(x) sum(abs(x))!=0)&result[,2]>0,])
    rownames(result) <- c(1:nrow(result))
    return(result)
  }
  if(shorten.result == FALSE){
    return(result)
  }
}
