## ---- load_copd----------------------------------------------------------
copd <- read.table("data/copd.txt", head=TRUE)
head(copd)

## ---- cox_pa-------------------------------------------------------------
library(survival)
cox.pa <- coxph(Surv(time.readmission, status.readmission) ~ 
                  as.factor(phys.act), data=copd)
cox.pa

## ---- cox_summary--------------------------------------------------------
summary(cox.pa)

## ---- plot_cox-----------------------------------------------------------
plot(survfit(cox.pa))

## ---- cox_mult-----------------------------------------------------------
cox.pa.adj <- coxph(Surv(time.readmission, status.readmission) ~
                      as.factor(phys.act) + age + fev + 
                      as.factor(smoke), data=copd)
cox.pa.adj

## ---- plot_mult----------------------------------------------------------
copd$smoke2 <- 1*(copd$smoke==1)
copd$smoke3 <- 1*(copd$smoke==2)
copd$phys.act2 <- 1*(copd$phys.act==1)
copd$phys.act3 <- 1*(copd$phys.act==2)

cox2.pa.adj <- coxph(Surv(time.readmission, status.readmission) ~
                       age + fev + smoke2 + smoke3 + phys.act2 +
                       phys.act3, data=copd)

newdata <- data.frame(age = mean(copd$age),
                      fev = mean(copd$fev, na.rm=TRUE),
                      smoke2 = 0, smoke3=0,
                      phys.act2=0, phys.act3=0)

plot(survfit(cox2.pa.adj, newdata= newdata), 
             xlab="Time (days)", 
             ylab="Probability of not being hospitalized") 

## ---- stepwise-----------------------------------------------------------
library(MASS)
cox.pa.adj

## ---- stepwise2----------------------------------------------------------
mod <- stepAIC(cox.pa.adj)

## ----stepwise3-----------------------------------------------------------
copd.complete <- copd[complete.cases(copd),]
cox.complete <- coxph(Surv(time.readmission, status.readmission) ~
                      as.factor(phys.act) + age + fev + 
                      as.factor(smoke), data=copd.complete)
mod <- stepAIC(cox.complete)

## ---- lrt----------------------------------------------------------------
cox1 <- coxph(Surv(time.readmission, status.readmission) ~
                      as.factor(phys.act) + age + as.factor(smoke),
                      data=copd.complete)
cox2 <- coxph(Surv(time.readmission, status.readmission) ~
                      as.factor(phys.act) + age + as.factor(smoke) +
                      fev, data=copd.complete)
anova(cox1, cox2)

## ---- wald---------------------------------------------------------------
cox2

## ---- cox_rm-------------------------------------------------------------
rm.cox2.pa.adj<- resid(cox2.pa.adj)
plot(rm.cox2.pa.adj)

## ---- cox_rm2------------------------------------------------------------
rd.cox2.pa.adj<- resid(cox2.pa.adj, type="deviance")
plot(rd.cox2.pa.adj)

## ---- plot_var_resid-----------------------------------------------------
cox.null <- coxph(Surv(time.readmission, status.readmission) ~ 1,
                  data = copd)
rm.cox.null <- resid(cox.null)
plot(copd$fev, rm.cox.null)
smooth <- lowess(copd$fev, rm.cox.null, delta=1)
lines(smooth, lty=2, lwd=2, col="red")

## ---- test_ph------------------------------------------------------------
cox.pa.adj.zph <- cox.zph(cox2.pa.adj)
cox.pa.adj.zph

## ---- fig.height=10, plot_ph---------------------------------------------
par(mfrow=c(3,2))
plot(cox.pa.adj.zph)

## ---- create_quant-------------------------------------------------------
copd$fev4 <- cut(copd$fev, 4)
table(copd$fev4)

## ---- cox.pa.strata------------------------------------------------------
cox.pa.strata <- coxph(Surv(time.readmission, status.readmission) ~ 
                   age + as.factor(smoke) + as.factor(phys.act) +
                   strata(fev4), data=copd)
cox.pa.strata

## ---- test_ph_strat------------------------------------------------------
cox.pa.strata.zph <- cox.zph(cox.pa.strata)
cox.pa.strata.zph

## ---- echo=FALSE---------------------------------------------------------
sessionInfo()

