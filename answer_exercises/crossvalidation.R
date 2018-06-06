data(oliveoil, package="pdfCluster")


ff <- function(i, method="lda") {
 X <- oliveoil[-i, -2]
 X.i <- oliveoil[i, -2]
 mm <- charmatch(method, c("lda", "randomforest"))
 if (mm==1) {
  mod <- lda(macro.area ~ . , data=X)
  ypred <- predict(mod, X.i)$class
 }  
 if (mm==2) {
  mod <- randomForest::randomForest(macro.area ~ . , data=X)
  ypred <- predict(mod, X.i, type="class")
 }  
 ypred
}

ans.lda <- unlist(lapply(1:nrow(oliveoil), FUN=ff))
tt <- table(ans.lda, oliveoil$macro.area)
flexclust::randIndex(tt)

ans.randomF <- unlist(lapply(1:nrow(oliveoil), FUN=ff, method="random"))
tt <- table(ans.randomF, oliveoil$macro.area)
flexclust::randIndex(tt)
