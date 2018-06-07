#
# Task 1
#
library(abn)

pp <- "c:/juan/CREAL/GitHub/Curso_R_avanzado/"
load(file.path(pp, "data_exercises/diet.Rdata"))

X <- cbind(X1[, c("casoc", "estudios", "peso", "mets_10a", "Colesterol")],
           X3)
X.c <- X[complete.cases(X),]

mydat <- X.c[, c("Colesterol", "gra_procmeat", "gra_oilolives", 
                 "gra_vegetables", "gra_fruit", "casoc")]

mydists <- list(Colesterol="binomial",
                gra_procmeat="gaussian",
                gra_oilolives="gaussian",
                gra_vegetables="gaussian",
                gra_fruit="gaussian",
                casoc="binomial")

mydag <- ggm::DAG(Colesterol~casoc+gra_procmeat, 
                  gra_vegetable~casoc+gra_fruit,
                  gra_oilolives ~ casoc, 
                  gra_procmeat ~ casoc)
mydag

model <- fitabn(dag.m=mydag, data.df = mydat, data.dists = mydists, 
                 centre=TRUE, create.graph = TRUE)

model$mlik

mydag0 <- mydag
mydag0[mydag==1] <- 0
mydag0

model0 <- fitabn(dag.m=mydag0, data.df = mydat, 
                 data.dists = mydists, 
                 centre=TRUE)

model0$mlik

# test to compare our model with NULL one
chi <- 2*(model$mlik - model0$mlik)
df <- sum(mydag) - sum(mydag0)
chi
df
pchisq(chi,1, lower.tail = FALSE)

library(graph)
plot(model$graph)


#
# Task 2
#

vars1 <- c("casoc", "sexo", "peso", "mets_10a", "Colesterol")
X <- cbind(X1[, vars1],  X3[,1:6])
mydat <- X[complete.cases(X),]
names(mydat)

dd <- c("binomial", "binomial", "gaussian", "gaussian", "binomial",
        rep("gaussian", 6))
dd
names(dd) <- c(vars1, names(X3)[1:6])
mydists <- as.list(dd)
mydists

ban <- retain <- matrix(0, nrow=ncol(mydat), ncol=ncol(mydat))
colnames(ban) <- rownames(ban) <- names(mydat)
colnames(retain) <- rownames(retain) <- names(mydat)

ii <- rep(5, ncol(mydat))
names(ii) <- names(mydat)
max.par <- as.list(ii)


mycache<-buildscorecache(data.df=mydat, data.dists=mydists,
                         dag.banned=ban, dag.retained=retain,
                         max.parents = max.par)
mp.dag<-mostprobable(score.cache=mycache)
mod.best <- fitabn(dag.m=mp.dag, data.df=mydat,
                   data.dists=mydists, create.graph=TRUE)

x11()
graph::plot(mod.best$graph, attrs=list(node=list(fontsize=30)))
