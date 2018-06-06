
library(nlme)

setwd("c:/juan/CREAL/github/Curso_R_avanzado/data_exercises/")

datos <- read.table("recuperainfarto.txt")
head(datos)
datos$id <- 1:nrow(datos)
datos.long <- reshape(datos, varying=c("V2", "V3", "V4", "V5","V6"), 
                      direction="long", idvar="id", sep="")
datos.long$time <- datos.long$time - 1
names(datos.long)[1] <- "hospital"
head(datos.long)

dim(datos)
dim(datos.long)


datos.s <- groupedData(V ~ time | id, datos.long)

plot(datos.s)
plot(datos.s, outer="hospital")


mod.lme <- lme(V ~ time * hospital, datos.s, random =  ~ 1)
summary(mod.lme)

mod.lme2 <- lme(V ~ time * hospital, datos.s)
anova(mod.lme, mod.lme2)

summary(mod.lme2)

plot(mod.lme2)

#
# Non-parametric
#

library(nparLD)

mod1 <- nparLD(V ~ time * hospital, data = datos.s,
               subject = "id", description = FALSE)

plot(mod1)

summary(mod1)

