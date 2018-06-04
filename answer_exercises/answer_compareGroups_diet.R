library(compareGroups)


getwd()
setwd("c:/juan/CREAL/GitHub/Curso_R_avanzado/data_exercises/")

load("diet.Rdata")
diet <- X1
names(diet)
table(diet$tipocancer)


# question 1
diet.cc <- subset(diet, tipocancer%in%c("Control", "Colorrectal", "Mama"))
table(diet.cc$tipocancer)
dim(diet.cc)

# questin 2
descr1 <- compareGroups(tipocancer ~ edad + sexo + estudios + peso +
                                    altura + mets_10a + mets_5a +
                                    Diabetes + Hipertensio + Colesterol,
                       data = diet.cc )
tab1 <- createTable(descr1)
tab1


# task 3
descr.colon <- compareGroups(tipocancer ~ . -id -casoc -casom -casop -casoe,
                        data = diet.cc,
                        subset = tipocancer%in%c("Control", "Colorrectal"))
tab.colon <- createTable(descr.colon, show.ratio=TRUE,
            show.p.overall=FALSE, show.p.trend = TRUE)
tab.colon
