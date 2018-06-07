library(rms)
n <- 1000    # define sample size
set.seed(17) # so can reproduce the results
age            <- rnorm(n, 50, 10)
blood.pressure <- rnorm(n, 120, 15)
cholesterol    <- rnorm(n, 200, 25)
sex            <- factor(sample(c('female','male'), n,TRUE))


# Specify population model for log odds that Y=1
L <- .4*(sex=='male') + .045*(age-50) +
  (log(cholesterol - 10)-5.2)*(-2*(sex=='female') + 2*(sex=='male'))
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
y <- ifelse(runif(n) < plogis(L), 1, 0)


ddist <- datadist(age, blood.pressure, cholesterol, sex)
options(datadist='ddist')


mod <- lrm(y ~ lsp(age,50)+sex*rcs(cholesterol,4)+blood.pressure)
nom <- nomogram(mod, fun=function(x)1/(1+exp(-x)),  # or fun=plogis
                fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                funlabel="Risk of Death")

plot(nom, xfrac=.45)

newIndiv <- data.frame(age=60, blood.pressure=110, 
                       cholesterol=200, sex="male")
predict(mod, newIndiv, type="fitted")
