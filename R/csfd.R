paired.comparison <- function(x, fact, pred, method = "jaccard"){
  x <- as.data.frame(t(x))
  if(missing(pred) == F)  pred <- as.data.frame(t(pred))
  if(method == "jaccard"){
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      x = ifelse((x==0), 0, 1)
      y = ifelse((y==0), 0, 1)
      sum(ifelse(((x+y)<2), 0, 1))/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1)))-sum(ifelse(((x+y)<2), 0, 1)))
    })))
    if(missing(pred) == FALSE){
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
        x = ifelse((x==0), 0, 1)
        y = ifelse((y==0), 0, 1)
        sum(ifelse(((x+y)<2), 0, 1))/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1)))-sum(ifelse(((x+y)<2), 0, 1)))
      })))
    }
  }
  if(method == "sorensen"){
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      x = ifelse((x==0), 0, 1)
      y = ifelse((y==0), 0, 1)
      sum <- sum(ifelse(((x+y)<2), 0, 1))
      2*sum/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1))))
    })))
    if(missing(pred) == FALSE){
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
      x = ifelse((x==0), 0, 1)
      y = ifelse((y==0), 0, 1)
      sum <- sum(ifelse(((x+y)<2), 0, 1))
      2*sum/((sum(ifelse(((x)<1), 0, 1)))+(sum(ifelse(((y)<1), 0, 1))))
    })))
    }
  }
  if(method == "renkonen"){
    x <- as.data.frame(apply(x, 2, function(x) abs(x)/sum(abs(x))))
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      sum(apply(data.frame(x, y), 1, min))
    })))
    if(missing(pred) == FALSE){
      pred <- as.data.frame(apply(pred, 2, function(x) abs(x)/sum(abs(x))))
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
        sum(apply(data.frame(x, y), 1, min))
      })))
    }
  }
  if(method == "log.sorensen"){
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      2*sum(apply(data.frame(x, y), 1, function(x) ifelse((min(x)==0|min(x)==0&max(x)==0), 0, log(min(x)+1)/log(max(x)+1))))/((sum(ifelse(((x)==0), 0, 1)))+(sum(ifelse(((y)==0), 0, 1))))
    })))
    if(missing(pred) == FALSE){
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
        2*sum(apply(data.frame(x, y), 1, function(x) ifelse((min(x)==0|min(x)==0&max(x)==0), 0, log(min(x)+1)/log(max(x)+1))))/((sum(ifelse(((x)==0), 0, 1)))+(sum(ifelse(((y)==0), 0, 1))))
      })))
    }
  }
  if(method == "braycurtis"){
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      m <- data.frame(x, y)
      1-(sum(abs(m[,1]-m[,2]))/sum(m[,1]+m[,2]))
    })))
    if(missing(pred) == FALSE){
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
        m <- data.frame(x, y)
        1-(sum(abs(m[,1]-m[,2]))/sum(m[,1]+m[,2]))
      })))
    }
  }
  if(method == "braycurtis2"){
    dc <- (as.vector(outer(x, x, Vectorize(function(x, y){
      m <- data.frame(x, y)
      1-sum(abs(m[,1]-m[,2]))/sum(m[,1]+m[,2])
    }))))^2
    if(missing(pred) == FALSE){
      pc <- (as.vector(outer(pred, pred, Vectorize(function(x, y){
        m <- data.frame(x, y)
        1-sum(abs(m[,1]-m[,2]))/sum(m[,1]+m[,2])
      }))))^2
    }
  }
  if(method == "canber"){
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      mutated <- data.frame(c(ifelse((abs(x)==0), 0, 1)), c(ifelse((abs(y)==0), 0, 1)))
      a <- sum(mutated[,1])
      b <- sum(mutated[,2])
      c <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
      1-((sum(abs((ifelse(x==0, 0.01, x))-(ifelse(y==0, 0.01, y)))/((ifelse(x==0, 0.01, x))+(ifelse(y==0, 0.01, y)))))*(1/(a+b-c)))
    })))
    if(missing(pred) == FALSE){
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
        mutated <- data.frame(c(ifelse((abs(x)==0), 0, 1)), c(ifelse((abs(y)==0), 0, 1)))
        a <- sum(mutated[,1])
        b <- sum(mutated[,2])
        c <- sum(ifelse(((mutated[,1]+mutated[,2])<2), 0, 1))
        1-((sum(abs((ifelse(x==0, 0.01, x))-(ifelse(y==0, 0.01, y)))/((ifelse(x==0, 0.01, x))+(ifelse(y==0, 0.01, y)))))*(1/(a+b-c)))
      })))
    }
  }
  if(method == "sqeudist"){
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      m <- data.frame(x, y)
      m = apply(m, 2, function(x) x/sum(x))
      1-sum((m[,1]-m[,2])^2)
    })))
    if(missing(pred) == FALSE){
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
        m <- data.frame(x, y)
        m = apply(m, 2, function(x) x/sum(x))
        1-sum((m[,1]-m[,2])^2)
      })))
    }
  }
  if(method == "morisita"){
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      2*(sum(x*y))/(((sum(x*(x-1)))/(sum(x)*(sum(x)-1))+(sum(y*(y-1)))/(sum(y)*(sum(y)-1)))*sum(x)*sum(y))
    })))
    if(missing(pred) == FALSE){
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
        2*(sum(x*y))/(((sum(x*(x-1)))/(sum(x)*(sum(x)-1))+(sum(y*(y-1)))/(sum(y)*(sum(y)-1)))*sum(x)*sum(y))
      })))
    }
  }
  if(method == "nxratio"){
    dc <- as.vector(outer(x, x, Vectorize(function(x, y){
      mean(apply(data.frame(x, y), 1, function(x) min(x)/max(x)), na.rm = T)
    })))
    if(missing(pred) == FALSE){
      pc <- as.vector(outer(pred, pred, Vectorize(function(x, y){
        mean(apply(data.frame(x, y), 1, function(x) min(x)/max(x)), na.rm = T)
      })))
    }
  }
  if(missing(fact) == F){
    f <- apply(fact, 2, function(x) as.vector(outer(as.numeric(x), as.numeric(x), '-')))
  }
  if(missing(fact) == T & missing(pred) == T) return(dc)
  if(missing(fact) == T) return(data.frame(dcom = dc, pcom = pc))
  if(missing(pred) == T) return(data.frame(dcom = dc, dif = f))
  if(missing(fact) == F & missing(pred) == F) return(data.frame(dcom = dc, pcom = pc, dif = f))
}
