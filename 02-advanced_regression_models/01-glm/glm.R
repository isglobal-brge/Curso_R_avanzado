## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE, size='footnotesize', fig.height=4)

## ----lm, echo=FALSE, fig.dim=c(3.5, 3.5)---------------------------------
library(xtable)
cal <- rnorm(100, 2800, 300)
chol <- -80 + 0.1*cal + rnorm(100, 20, 26)
plot(cal, chol, xlab="Calories consumption", ylab="Cholesterol level")
abline(lm(chol~cal), lwd=2, col="blue")

## ----lm_mod--------------------------------------------------------------
data(airquality)
head(airquality)

## ----lmModS--------------------------------------------------------------
mod <- lm(Ozone ~ Temp, data=airquality)
summary(mod)

## ----lmModM--------------------------------------------------------------
mod <- lm(Ozone ~ Solar.R + Wind + Temp + as.factor(Month),
          data=airquality)
summary(mod)

## ----lmModF, echo=FALSE, fig.dim=c(3.5, 3.5)-----------------------------
boxplot(Ozone  ~ Month, data=airquality)

## ----lmModVal------------------------------------------------------------
par(mfrow=c(2,2))
plot(mod)

## ----lmModPlot, echo=FALSE, fig.dim=c(3.5, 3.5)--------------------------
mod <- lm(Ozone ~ Temp, data=airquality)
plot(airquality$Temp, airquality$Ozone,
        xlab="Temperature", ylab="Ozone")

## ----find_trans----------------------------------------------------------
require(car)
trans <- powerTransform(mod)
trans

## ----linerization--------------------------------------------------------
Ozone.trans <- bcPower(airquality$Ozone,
                       coef(trans, round=TRUE))

mod.trans <- lm(Ozone.trans ~ Temp, data=airquality)

## ----linerizationFig, fig.dim=c(3, 3)------------------------------------
plot(Ozone.trans ~ Temp, data=airquality)
abline(mod.trans, col="blue")

## ----summary_lm----------------------------------------------------------
summary(mod)

## ----summary_trans-------------------------------------------------------
summary(mod.trans)

## ----lung----------------------------------------------------------------
data(ovarian, package="survival")
head(ovarian)

## ----modL----------------------------------------------------------------
mod2 <- glm(fustat ~ rx, data=ovarian, family="binomial")
summary(mod2)

## ----step----------------------------------------------------------------
modAll <- glm(fustat ~ ., data=ovarian, family="binomial")
modBest <- MASS::stepAIC(modAll)

## ----stepsumm------------------------------------------------------------
summary(modBest)

## ----get_lung------------------------------------------------------------
breast <- read.delim("../../data/breastCat.txt")
head(breast)

## ----rates_evol----------------------------------------------------------
breast$rate <- (breast$deaths/breast$population)*100000
plot(breast$year, breast$rate, xlab="Year",
     ylab="Mortality rate per 100,000 hab",
     type="n")
points(breast$year, breast$rate, pch=16, col="blue")

## ----poisson_model-------------------------------------------------------
modPoisson <- glm(deaths ~ year + offset(log(population)),
               family=poisson, data=breast)
summary(modPoisson)

## ----change--------------------------------------------------------------
round((exp(modPoisson$coef[2]) - 1)*100, 2)

## ----predict-------------------------------------------------------------
counts.pred <- predict(modPoisson, type="response")
tasa.pred <- (counts.pred/breast$population)*100000
plot(breast$year, breast$rate, xlab="Year",
     ylab="Mortality rate per 100,000 hab",
     type="n")
points(breast$year, breast$rate, pch=16, col="blue")
lines(breast$year, tasa.pred, lwd=2, lty=2, col="red")

## ----get_pvalue----------------------------------------------------------
1 - pchisq(modPoisson$deviance, modPoisson$df.res)
modPoisson

## ----neg_binom_fit-------------------------------------------------------
library(MASS)
modNB <- glm.nb(deaths ~ year + offset(log(population)),
                data=breast)
summary(modNB)

## ----ljr-----------------------------------------------------------------
library(ljr)
ljrk(1, breast$deaths, breast$population, breast$year+.5)

## ----ljr_test------------------------------------------------------------
ljrjk(0, 1, breast$deaths, breast$population, breast$year+.5,
      R = 1000)

## ----changes-------------------------------------------------------------
mod <- ljrk(1, breast$deaths, breast$population, breast$year+.5)

cbind(year=c(1975, mod$Joinpoints),
      APC=round((exp(mod$Coef[-1])-1)*100,2))

