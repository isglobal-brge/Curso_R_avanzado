dispe <- read.delim("c:/Juan/CREAL/GitHub/Curso_R_avanzado/data_exercises/dispe.txt")
head(dispe)

mod <- glm(diagfina ~ tabac, data=dispe, family="binomial")
summary(mod)
summary(mod)$coefficients[2,4]
r2 <- DescTools::PseudoR2(mod, "Nagelkerke")
round(r2*100, 1)

names(dispe)

modAll <- glm(diagfina ~ . - cas - esofagit - ulcpep - score,
              data=dd, family="binomial")


dd <- dispe[ , 2:21] # select only those variables important for the model
modAll <- glm(diagfina ~ ., data=dd, family="binomial")
modBest <- MASS::stepAIC(modAll)
summary(modBest)
DescTools::PseudoR2(modBest, "Nagelkerke")

or <- exp(coef(modAll)[-1])
or
ci <- exp(confint(modAll)[-1, ])
ci

ans <- round(cbind(or, ci),2)
ans


mm <- glm(diagfina ~ score, data=dispe, family="binomial")
DescTools::PseudoR2(mm, "Nagelkerke")

exp(coef(mm)[2])

# No efficient
ans <- matrix(NA, nrow=20, ncol=2)
for (i in 2:20) {
  mod <- glm(diagfina ~ dispe[,i], data=dispe, family="binomial")
  ss <- summary(mod)
  p <- ss$coefficients[2,4]
  r2 <- DescTools::PseudoR2(mod, "Nagelkerke")
  r2 <- round(r2*100, 1)
  ans[i,] <- c(r2, p)
}

# complicated
ff <- function(i){
  mod <- glm(diagfina ~ dispe[,i], data=dispe, family="binomial")
  ss <- summary(mod)
  p <- ss$coefficients[2,4]
  r2 <- DescTools::PseudoR2(mod, "Nagelkerke")
  r2 <- round(r2*100, 1)
  ans <- c(r2, p)
  ans
}

lapply(2:20, ff)

# efficient
ff2 <- function(x, y){
  mod <- glm(y ~ x, data=dispe, family="binomial")
  ss <- summary(mod)
  p <- ss$coefficients[2,4]
  r2 <- DescTools::PseudoR2(mod, "Nagelkerke")
  r2 <- round(r2*100, 1)
  ans <- c(formatC(r2), formatC(p))
  names(ans) <- c("R2", "p-value")
  ans
}




out <- lapply(dispe[,2:20], ff2, y=dispe$diagfina)
out


ff3 <- function(x, y, adj){
  mod <- glm(y ~ x, data=dispe, family="binomial")
  mod <- update(mod, paste(" ~ . + ", adj))
  ss <- summary(mod)
  p <- ss$coefficients[2,4]
  r2 <- DescTools::PseudoR2(mod, "Nagelkerke")
  r2 <- round(r2*100, 1)
  ans <- c(formatC(r2), formatC(p))
  names(ans) <- c("R2", "p-value")
  ans
}

out <- lapply(dispe[,5:20], ff3, y=dispe$diagfina, adj="edatr + sexe")
out




