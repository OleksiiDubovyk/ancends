ccompare <- function(data1, data2, method = "jaccard", just.index = TRUE){
  if(missing(data2)){
    mutated <- as.data.frame(ifelse((abs(data1)==0), 0, 1))
    if(ncol(data1)>2) {
      stop("Must be two objects for paired comparison")
    }
    a <- sum(ifelse((mutated[,1]==0), 0, 1))
    b <- sum(ifelse((mutated[,2]==0), 0, 1))
    c <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
    d <- sum(ifelse(((mutated[,1]+mutated[,2])==1), 1, 0))
    k <- a+b-c
  }else{
    data <- data.frame(A=data1, B=data2)
    if(ncol(data)>2) {
      stop('Must be exactly two vector objects')
    }else{
      mutated <- as.data.frame(ifelse((abs(data)==0), 0, 1))
      a <- length(data1[data1>0])
      b <- length(data1[data2>0])
      c <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
      d <- sum(ifelse(((mutated[,1]+mutated[,2])==1), 1, 0))
      k <- a+b-c
    }
  }
  coefficients <- c(a, b, c, d, k)
  names(coefficients) <- c('A', 'B', 'c, in common', 'd, different', 'k, general')
  if(method=="jaccard"){
    K <- c/(a+b-c)
  }
  if(method=="sorensen"){
    K <- 2*c/(a+b)
  }
  if(method=="kulczinski"){
    K <- (c/2)*((1/a)*(1/b))
  }
  if(method=="ochiai"){
    K <- c/sqrt(a*b)
  }
  if(method=="dice"){
    K <- c/min(a, b)
  }
  if(method=="bblanquet"){
    K <- c/max(a, b)
  }
  if(method=="baroni"){
    K <- (sqrt(c*d)+c)/(sqrt(c*d)+a+b-c)
  }
  if(method=="mountford"){
    K <- (2*c)/((2*a*b)-((a+b)*c))
  }
  if(just.index == F){
   return(list(species = coefficients, method = method, statistic = K, presence_absence_table = mutated))
  }else{
    return(K)
  }
}
