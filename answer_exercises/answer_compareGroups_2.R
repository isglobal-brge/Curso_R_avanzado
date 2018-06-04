#
# Exercise 2
#

compareGroups <- function(x, y, pvalue=TRUE){

  if(missing(x) | missing(y))
    stop("x and y are required")

  if(nrow(x)!=length(y))
    stop("x and y should have the same number of observations")

  if(!is.factor(y) | length(levels(y))!=2)
    stop("y must be a 2 level factor")

  ans <- list()

  for (i in 1: ncol(x)){
    xx <- x[,i]
    ans[[i]] <- list()
    ans[[i]]$table <- descriptive(xx, y)
    if (pvalue){
      ans[[i]]$pvalue <- pval(xx, y)
    }
  }
  names(ans) <- colnames(x)
  class(ans) <- "compareGroups"
  ans
}

out <- compareGroups(dd, group)

print.compareGroups <- function(x, n=10, sig=0.05, ...){

  ff <- function(x){
    x$pval
  }
  pvalues <- unlist(lapply(x, FUN=ff))

  if (length(pvalues)<n){
   cat("P-values are: \n")
   cat(pvalues)
  }
  else{
    varsig <- names(pvalues[pvalues<sig])
    cat("significant variables are: \n")
    cat(varsig)
  }
}

out
