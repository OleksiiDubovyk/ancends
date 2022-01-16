compare.communities <- function(com1, com2, nperm = 1000, distance = ancends::jaccard, show.hist = FALSE){
  fnct <- distance
  resample.communities <- function(com1 = com1, com2 = com2){
    df <- data.frame(com1 = com1, com2 = com2)
    pool <- unlist(sapply(1:nrow(df), function(i){
      sapply(1:2, function(j){
        rep(rownames(df[i,]), df[i, j])
      })
    }))
    n1 <- sum(df[1])
    n2 <- sum(df[2])
    perm1 <- sample(x = pool, size = n1)
    perm2 <- sample(x = pool, size = n2)
    pseudo1 <- sapply(rownames(df), function(i){
      sum(sapply(perm1, function(j){
        j == i
      }))
    })
    pseudo2 <- sapply(rownames(df), function(i){
      sum(sapply(perm2, function(j){
        j == i
      }))
    })
    df <- data.frame(pseudo1, pseudo2)
    fnct(df)
  }
  distr <- numeric(nperm)
  for (i in 1:nperm){
    distr[i] <- resample.communities(lbt[,1], lbt[,2])
  }
  metric <- fnct(data.frame(com1 = com1, com2 = com2))
  rslt <- c(round(metric, 3), ifelse(sum(distr <= metric)/nperm == 0,
                                     paste("<", 1/nperm),
                                     sum(distr <= metric)/nperm))
  names(rslt) <- c("Metric", "p-value")
  if (show.hist){
    hist(distr, main = paste("Metric = ", rslt[1], "; p-value = ", rslt[2], sep = ""),
         freq = F, xlab = "Metric", xlim = c(0, 1))
    lines(density(distr), col = "blue")
    abline(v = rslt[1], col = "red", lwd = 3)
  }else{
    rslt
  }
}
