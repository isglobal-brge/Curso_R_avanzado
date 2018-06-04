## ---- get_data-----------------------------------------------------------
library(survival)
datos <- read.table("data/sauerbre.txt", header=TRUE)
head(datos)

## ---- surv, eval=FALSE---------------------------------------------------
## Surv(datos$time, datos$event)

## ---- surv2, echo=FALSE--------------------------------------------------
Surv(datos$time, datos$event)[1:40]

## ---- km-----------------------------------------------------------------
ans <- survfit(Surv(time, event)~1, datos)
ans

## ---- table_km-----------------------------------------------------------
summary(ans)

## ---- plotkm-------------------------------------------------------------
plot(ans, xlab="Time", ylab="Survival probability")

## ---- km_twogroups-------------------------------------------------------
ans.ther <- survfit(Surv(time, event)~as.factor(therapy), datos)
ans.ther

## ---- logrankplot--------------------------------------------------------
plot(ans.ther)

## ---- logrankplot2-------------------------------------------------------
plot(ans.ther, xlab="Time", ylab="Survival probability", 
               col=c("red", "blue"))
legend("bottomleft", c("No treatment", "Treatment"),
               col=c("red", "blue"), lty=c(1,1))

## ---- logrank------------------------------------------------------------
ans.logrank<-survdiff(Surv(time, event)~as.factor(therapy), datos,
                              rho=0) 
ans.logrank

## ---- wilcox-------------------------------------------------------------
ans.wilcox<-survdiff(Surv(time, event)~as.factor(therapy), datos,
                              rho=1) 
ans.wilcox

## ---- stratified---------------------------------------------------------
ans.strat<-survdiff(Surv(time, event)~as.factor(therapy)
                      +strata(meno.status), datos)
ans.strat

ans.logrank

## ---- trend--------------------------------------------------------------
ans.grade<-survfit(Surv(time, event)~as.factor(tumor.grade), datos)
plot(ans.grade, xlab="Time", ylab="Survival probability", col=1:3)
legend("bottomleft", c("grado 1", "grado 2", "grado 3"), 
                     col=1:3, lty=1)

## ---- trend2-------------------------------------------------------------
survdiff(Surv(time, event)~as.factor(tumor.grade), datos)

## ---- echo=FALSE---------------------------------------------------------
sessionInfo()

