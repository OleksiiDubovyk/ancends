centered.comparison <- function(x, method = "eudist"){
  centroid <- as.data.frame(apply(x, 2, mean))
  if(method == "jaccard"){
    comparison <- apply(x, 1, function(x) {
      x = ifelse((x==0), 0, 1)
      y = ifelse((centroid==0), 0, 1)
      sum(ifelse(((x+y)<2), 0, 1))/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1)))-sum(ifelse(((x+y)<2), 0, 1)))
    })
  }
  if(method == "sorensen"){
    comparison <- apply(x, 1, function(x) {
      x = ifelse((x == 0), 0, 1)
      y = ifelse((centroid == 0), 0, 1)
      c <- sum(ifelse(((x + y)<2), 0, 1))
      2*c/((sum(ifelse(((x) < 1), 0, 1)))+(sum(ifelse(((y) < 1), 0, 1))))
    })
  }
  if(method == "euclidian"){
    comparison <- apply(x, 1, function(x) {
      m <- data.frame(x, centroid)
      sqrt(sum((m[,1]-m[,2])^2))
    })
  }
  if(method == "sqeudist"){
    comparison <- apply(x, 1, function(x) {
      m <- data.frame(x, centroid)
      m = apply(m, 2, function(x) x/sum(x))
      1-sum((m[,1]-m[,2])^2)
    })
  }
  if(method == "chord"){
    comparison <- apply(x, 1, function(x) {
      sqrt(sum(((x/sqrt(sum(x^2)))-(centroid/sqrt(sum(centroid^2))))^2))
    })
  }
  if(method == "chisq.m"){
    comparison <- apply(x, 1, function(x){
      m <- data.frame(x, centroid)
      m = apply(m, 2, function(x) x/sum(x))
      sqrt(sum((1/(x+centroid))*((m[,1]-m[,2])^2)))
    })
  }
  if(method == "chisq.d"){
    comparison <- apply(x, 1, function(x){
      m <- data.frame(x, centroid)
      m = apply(m, 2, function(x) x/sum(x))
      sqrt(sum((1/(x+centroid))*((m[,1]-m[,2])^2)))*sqrt(sum(x)+sum(centroid))
    })
  }
  if(method == "hellinger"){
    comparison <- apply(x, 1, function(x){
      m <- data.frame(x, centroid)
      m = apply(m, 2, function(x) x/sum(x))
      sqrt(sum((sqrt(m[,1])-sqrt(m[,2]))^2))
    })
  }
  if(method == "braycurtis"){
    comparison <- apply(x, 1, function(x){
      1-(sum(abs(x-centroid))/sum(x+centroid))
    })
  }
  if(method == "morisita"){
    comparison <- apply(x, 1, function(x){
      (2*sum(x*centroid))/(((sum(x*(x-1))/(sum(x)*(sum(x)-1)))+(sum(centroid*(centroid-1))/(sum(centroid)*(sum(centroid)-1))))*sum(x)*sum(centroid))
    })
  }
  if(method == "manhattan"){
    comparison <- apply(x, 1, function(x){
      sum(abs(x-centroid))
    })
  }
  if(method == "manhattan.pc"){
    comparison <- apply(x, 1, function(x){
      m <- data.frame(x, centroid)
      m = apply(m, 2, function(x) x/sum(x))
      sum(abs(m[,1]-m[,2]))
    })
  }
  if(method == "canberra"){
    comparison <- apply(x, 1, function(x){
      x = ifelse((x==0), 0, 1)
      y = ifelse((centroid==0), 0, 1)
      m <- data.frame(x, centroid)
      sum((abs(m[,1]-m[,2]))/(m[,1]+m[,2]))*(1/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1)))-sum(ifelse(((x+y)<2), 0, 1))))
    })
  }
  if(method == "renkonen"){
    comparison <- apply(x, 1, function(x){
      m <- data.frame(x, centroid)
      m = apply(m, 2, function(x) x/sum(x))
      sum(apply(m, 1, min))
    })
  }
  if(method == "nxratio"){
    comparison <- apply(x, 1, function(x){
      mean(apply(data.frame(x, centroid), 1, function(x) min(x)/max(x)), na.rm = T)
    })
  }
  if(method == "nxratio.pc"){
    comparison <- apply(x, 1, function(x){
      m <- data.frame(x, centroid)
      m = apply(m, 2, function(x) x/sum(x))
      mean(apply(m, 1, function(x) min(x)/max(x)), na.rm = T)
    })
  }
  if(method == "log.sorensen"){
    comparison <- apply(x, 1, function(x){
      2*sum(apply(data.frame(x, centroid), 1, function(x) ifelse((min(x)==0|min(x)==0&max(x)==0), 0, log(min(x)+1)/log(max(x)+1))))/
        ((sum(ifelse(((x)==0), 0, 1)))+(sum(ifelse(((centroid)==0), 0, 1))))
    })
  }
  return(comparison)
}
