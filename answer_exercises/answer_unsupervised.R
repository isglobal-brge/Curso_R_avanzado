library(made4)

#
# task 1
#

setwd("c:/Juan/CREAL/GitHub/Curso_R_Avanzado")
load("data_exercises/nhanes.Rdata")
ls()

# select variables
dd <- nhanes.nut[,1:29]
group <- as.factor(nhanes.nut$CHOL2)
table(group)


# scale data
dd.s <- lapply(dd, scale)

#coa
mod <- ord(dd, type="coa", trans=TRUE, classvec = group)
plot(mod, nlab=5)

# top variables second axis (because it seems that fist axis is not separating
# normal vs hypercol
topgenes(mod, axis=2, end="pos")
topgenes(mod, axis=2, end="neg")

# Let us investigate whether there is association between second axis and CHOL2 (variable group)
pca1 <- out$ord$co[,2]
mod <- glm(group ~ pca1, family="binomial")
summary(mod)


#
# task 2
#

library(made4)
setwd("c:/Juan/CREAL/GitHub/biomarkers_multiple_tables")
load("data_exercises/nci60.Rdata")
miRNA <- nci60$miRNA

pca.miRNA <- ord(miRNA, classvec=cancer)
plot(pca.miRNA, nlab=5)

# these are associated with Leukemia 
# (notice in the figure that in this case leukemias are in the negative side)
topgenes(pca.miRNA, axis=1, n=5, ends="neg")

# these are associated with Melanoma
# (notice in the figure that in this case melanomas are in the negative side)
ax1.pos <- topgenes(pca.miRNA, axis=1, n=5, ends="pos")
ax1.pos
ax2.pos <- topgenes(pca.miRNA, axis=2, n=5, ends="pos")
intersect(ax1.pos, ax2.pos)

# these are associated with Central Nervous System
topgenes(pca.miRNA, axis=2, n=5, ends="neg")

# variability
summary(pca.miRNA$ord)


plot(pca.miRNA, axis1 = 3, axis2=4)

#
# task 3
#

library(cluster)
setwd("c:/Juan/CREAL/GitHub/Curso_R_avanzado/")
load("data_exercises/diet.Rdata")


X <- cbind(X2, X3)
X <- log(X + 0.0001)
any(!sapply(X, is.numeric)) #check whether any variable is not numeric
names(X)

sel <- complete.cases(X)
X <- X[sel, ]
median <- apply(X, 2, median, na.rm=TRUE)
mads <- apply(X, 2, mad, na.rm=TRUE)
X.s <- scale(X, center=median, scale = mads)
dd <- dist(X.s)
dd.h <- hclust(dd)
plot(dd.h)

# silhouette
par(mfrow=c(2,2))
for (i in 2:5)
  plot(silhouette(cutree(dd.h,i), dd))

gg <- as.factor(cutree(dd.h, 2))
table(gg)


XX.complete <- X1[sel,]

prop.table(table(gg, XX.complete$casoc),1)
prop.table(table(gg, XX.complete$tipocancer),1)

prop.table(table(gg, XX.complete$tipocancer, XX.complete$sexo),1)

XX.complete$gg <- gg
ans <- compareGroups::compareGroups(gg ~ ., XX.complete)
compareGroups::createTable(ans)

