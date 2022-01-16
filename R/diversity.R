diversity <- function(data, transposed = FALSE){
  if(transposed == TRUE) data = t(data)
  if(ncol(data)>1){
    Simpson <- apply(data, 2, function (data) 1-sum((data/sum(data))^2))
    Shannon <- apply(data, 2, function (data) -sum((data[data>0]/sum(data))*log(data[data>0]/sum(data))))
    Evenness <- apply(data, 2, function (data) (2.71828182845904523536^(-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data)))))/length(data[data>0]))
    Brillouin <-  apply(data, 2, function(data) ((log(factorial(sum(data))))-sum(log(factorial(data))))/sum(data) )
    Menhinick <- apply(data, 2, function (data) length(data[data>0])/sqrt(sum(data)))
    Margalef <- apply(data, 2, function (data) (length(data[data>0])-1)/log(sum(data)))
    Equitability <- apply(data, 2, function (data) (-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data))))/log(length(data[data>0])))
    BergerParker <- apply(data, 2, function (data) max(data)/sum(data))
    result <- data.frame(Simpson, Shannon, Evenness, Brillouin, Menhinick, Margalef, Equitability, BergerParker)
    result <- t(result)
  }
  if(ncol(data)==1){
    Simpson <- 1-sum((data/sum(data))^2)
    Shannon <- -sum((data[data>0]/sum(data))*log(data[data>0]/sum(data)))
    Evenness <- (2.71828182845904523536^(-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data)))))/length(data[data>0])
    Brillouin <-  ((log(factorial(sum(data))))-sum(log(factorial(data))))/sum(data)
    Menhinick <- length(data[data>0])/sqrt(sum(data))
    Margalef <- (length(data[data>0])-1)/log(sum(data))
    Equitability <- (-sum((data[data>0]/sum(data))*log(data[data>0]/sum(data))))/log(length(data[data>0]))
    BergerParker <- max(data)/sum(data)
    result <- data.frame(Diversity = c(Simpson, Shannon, Evenness, Brillouin, Menhinick, Margalef, Equitability, BergerParker))
    rownames(result) <- c('Simpson', 'Shannon', 'Evenness', 'Brillouin', 'Menhinick', 'Margalef', 'Equitability', 'Berger_Parker')
  }
  return(result)
}
