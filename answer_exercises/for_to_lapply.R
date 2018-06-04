
for (i in 1: ncol(dd)){
  xx <- dd[,i]
  ans[[i]] <- list()
  ans[[i]]$table <- descriptive(xx, y)
  if (pvalue){
    ans[[i]]$pvalue <- pval(xx, y)
  }
}

ff <- function(x, y){
  desc <- descriptive(x,y)
  p <- pval(x, y)
  ans <- list(desc, p)
  ans
}

ff2 <- function(i, X, y){
  x <- X[,i]
  desc <- descriptive(x,y)
  p <- pval(x, y)
  ans <- list(desc, p)
  ans
}


lapply(dd, FUN=ff, y=group)
lapply(1:ncol(X), FUN=ff2, X=dd, y=group)
