## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(tidy=FALSE, size='footnotesize', fig.height=4)

## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
concordance=TRUE
)

## ----setup, echo=FALSE---------------------------------------------------
options(width = 80)

## ----leeDatos------------------------------------------------------------
datos <- read.table("../../data/hypothetical_largo.txt", header=TRUE)
datos[1:12,]

## ----libreria------------------------------------------------------------
library(gee)

## ----argumentos----------------------------------------------------------
args(gee)

## ----correlacion, results='hide'-----------------------------------------
mod.gee.indep <- gee(score ~ group + time,
                     data = datos, id = id,
                     family = gaussian,
                     corstr = "independence")

## ----correlacion2, results='hide'----------------------------------------
mod.gee.AR <- gee(score ~ group + time,
                  data = datos, id = id,
                  family = gaussian,
                  corstr = "AR-M")

## ----summary-------------------------------------------------------------
ss.indep <- summary(mod.gee.indep)
ss.AR <- summary(mod.gee.AR)
names(ss.AR)

## ----compare_coef--------------------------------------------------------
ss.indep$coef
ss.AR$coef

## ------------------------------------------------------------------------
ss.indep$working.correlation
ss.AR$working.correlation

## ----carga libreria------------------------------------------------------
library(nlme)

## ----estructura----------------------------------------------------------
datos.s <- groupedData(score ~ time | id, datos)
head(datos.s)

## ----grafica-------------------------------------------------------------
plot(datos.s)

## ----modelo lme----------------------------------------------------------
mod.lme <- lme(score ~ time + group, datos.s, random =  ~ 1)
mod.lme

## ----modelo lineal-------------------------------------------------------
mod.lm <- lm(score ~ time + group, datos)
summary(mod.lm)

## ----modelo lme2---------------------------------------------------------
mod.lme2 <- lme(score ~ time + group, datos.s)

## ----anova---------------------------------------------------------------
anova(mod.lme, mod.lme2)

## ----verificar-----------------------------------------------------------
plot(mod.lme)

## ----np------------------------------------------------------------------
library(nparLD)
data(Orthodont, package="nlme")
head(Orthodont)

## ----plotNP--------------------------------------------------------------
boxplot(distance ~ age, data = Orthodont, lwd = 2, xlab = "time",
    font.lab = 2, cex.lab = 2, main = "Box Plots")

## ----NPfit---------------------------------------------------------------
mod0 <- nparLD(distance ~ age, data = Orthodont,
                  subject = "Subject", description = FALSE)

## ----plotNPfit-----------------------------------------------------------
plot(mod0)

## ----NPfit2--------------------------------------------------------------
mod1 <- nparLD(distance ~ age*Sex , data = Orthodont,
                  subject = "Subject", description = FALSE)

## ----plotNPfit2----------------------------------------------------------
plot(mod1)

## ----NPsum---------------------------------------------------------------
summary(mod1)

