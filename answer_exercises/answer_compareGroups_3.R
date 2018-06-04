#
# Exercise 3
#

d <- 3
ans <- NULL
for (i in 1:length(out)) {
dd.i <- out[[i]]$table
grA <- paste0(dd.i[1,2], "(", round(dd.i[1,3], d), ")")
grB <- paste0(dd.i[2,2], "(", round(dd.i[2,3], d), ")")
p <- out[[i]]$pvalue 
ans.i <- c(grA, grB, formatC(p))
ans <- rbind(ans, ans.i)
}

































