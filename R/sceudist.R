sceudist <- function(data, log.transform = TRUE, log.base, squared = FALSE){
  e <- exp(1)
  if(log.transform == TRUE){
    if(missing(log.base)){
      if(squared == FALSE){
        if(ncol(data) > 2){
          as.data.frame(outer(data, data, Vectorize(function(x, y){
            1 - log(e*sqrt((sum((x-y)^2))/(1+sum((x-y)^2))))
          })))
        }
        if(ncol(data) == 2){
          1 - log(e*sqrt((sum((data[,1]-data[,2])^2))/(1+sum((data[,1]-data[,2])^2))))
        }
      }else{
        if(ncol(data) > 2){
          as.data.frame(outer(data, data, Vectorize(function(x, y){
            1 - log((e*sum((x-y)^2))/(1+sum((x-y)^2)))
          })))
        }
        if(ncol(data) == 2){
          1 - log((e*sum((data[,1]-data[,2])^2))/(1+sum((data[,1]-data[,2])^2)))
        }
      }
    }else{
      if(squared == FALSE){
        if(ncol(data) > 2){
          as.data.frame(outer(data, data, Vectorize(function(x){
            1 - log((log.base*sqrt((sum((x-y)^2))/(1+sum((x-y)^2)))), log.base)
          })))
        }
        if(ncol(data) == 2){
          1 - log((log.base*sqrt((sum((data[,1]-data[,2])^2))/(1+sum((data[,1]-data[,2])^2)))), log.base)
        }
      }else{
        if(ncol(data) > 2){
          as.data.frame(outer(data, data, Vectorize(function(x, y){
            1 - log(((log.base*sum((x-y)^2))/(1+sum((x-y)^2))), log.base)
          })))
        }
        if(ncol(data) == 2){
          1 - log(((log.base*sum((data[,1]-data[,2])^2))/(1+sum((data[,1]-data[,2])^2))), log.base)
        }
      }
    }
  }else{
    if(squared == FALSE){
      if(ncol(data) > 2){
        as.data.frame(outer(data, data, Vectorize(function(x, y){
          1 - sqrt((sum((x-y)^2))/(1+sum((x-y)^2)))
        })))
      }
      if(ncol(data) == 2){
        1 - sqrt((sum((data[,1]-data[,2])^2))/(1+sum((data[,1]-data[,2])^2)))
      }
    }else{
      if(ncol(data) > 2){
        as.data.frame(outer(data, data, Vectorize(function(x, y){
          1 - ((sum((x-y)^2))/(1+sum((x-y)^2)))
        })))
      }
      if(ncol(data) == 2){
        1 - ((sum((data[,1]-data[,2])^2))/(1+sum((data[,1]-data[,2])^2)))
      }
    }
  }
}
