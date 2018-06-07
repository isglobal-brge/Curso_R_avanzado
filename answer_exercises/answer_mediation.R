setwd("c:/Juan/CREAL/GitHub/Curso_R_avanzado/data_exercises/")
load("lusc.Rdata")
head(lusc)

mod1 <- glm(Cancer ~ LOY + age, data=lusc, family="binomial")
summary(mod1)

mod2 <- glm(Cancer ~ LOY + age + UTY, data=lusc, 
            family="binomial")
summary(mod2)

library(mediation)
ans <- mediate(mod1, mod2, treat="LOY", mediator="UTY")
summary(ans)


library(abn)
mydat <- lusc[,c("LOY", "DDX3Y", "age", "Cancer")]
mydists <- lapply(mydat, function(x) ifelse(is.factor(x), "binomial", "gaussian"))

ban <- retain <- matrix(0, nrow=ncol(mydat), ncol=ncol(mydat))
colnames(ban) <- rownames(ban) <- names(mydat)
colnames(retain) <- rownames(retain) <- names(mydat)

max.par <- lapply(mydat, function(x) 3)


mycache<-buildscorecache(data.df=mydat, data.dists=mydists,
                         dag.banned=ban, dag.retained=retain,
                         max.parents = max.par)
mp.dag<-mostprobable(score.cache=mycache)
mod.best <- fitabn(dag.m=mp.dag, data.df=mydat,
                   data.dists=mydists, create.graph=TRUE)

x11()
graph::plot(mod.best$graph, attrs=list(node=list(fontsize=20)))


