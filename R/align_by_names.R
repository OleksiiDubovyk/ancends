align.by.names <- function(l, replacement = 0){
  lnames <- numeric(0)
  for (i in 1:length(l)){
    lnames <- c(lnames, names(l[[i]]))
  }
  lnames <- unique(lnames)
  m <- matrix(replacement, nrow = length(lnames), ncol = length(l))
  colnames(m) <- 1:length(l)
  rownames(m) <- lnames
  for (i in 1:length(l)){
    for (j in names(l[[i]])){
      m[j, i] <- l[[i]][j]
    }
  }
  m
}
