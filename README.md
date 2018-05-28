# Advanced R course: advanced statistical methods applied to Biomedicine

4-days course introducing R building packages, advanced and multivariate statistical methods applied to biomedical problems. 
Course organized at Instituto Aragonés de Ciencias de la Salud (IACS), Centro de Investigación Biomédica de Aragón (CIBA), 
4-7 June, 2018.


**Lecturer**: Juan R Gonzalez, Associate Research Professor, Head of Bioinformatics Research Group in Genetic Epidemiology (BRGE) of
Barcelona Institute for Global Health (ISGlobal) and Adjunct Professor to the Mathematics 
Department of Universitat Autonoma de Barcelona (UAB)

- **url:** [BRGE](http://brge.isglobal.org)
- **GitHub:** [GitHub BRGE](https://github.com/isglobal-brge)
- **email:** juanr.gonzalez@isglobal.org

## Introduction

Biomedical studie are generating a large amount of data. Most of these data are complex and, therefore, adavance statistical
models are required to obtain proper scientific knowledge. The R statistical software is a powerfull tool which includes several
packages to address most of the methods required in complex studies. R also allows the creation of new packages implementing
required functions of new methodologies to be shared with the scientific community in a organized and standard way.

## Methodology

- Lectures will briefly introduce statistical methods required to address specific scientific questions and how to analyze 
real data using R pakcages. The methods will be illustrated using real data from different settigs including epidemiology, 
nutrition, air pollution, genetic, genomics among others. 

- Excersises will analyze data from a case/control study in cancer setting were controls and 4 types of cancer (colorectal, stomach, breast and prostate) were studied
  

## Outline

- **Day 1:** Creating R packages.
- **Day 2:** Advanced regression models: logistic, poisson and negative binomial regression, joinpoint and segmented regression, survival analysis, linear mixed models.
- **Day 3:** Multivariate methods for one table (non-supervised / supervised).
- **Day 4:** DAGs and mediation analysis.

## Preparing Rstudio

- Install R (version 3.5.0): https://cloud.r-project.org/
- Install Rstudio: https://www.rstudio.com/
- Install Rtools (install it in a Folder called c:/Rtools): https://cran.r-project.org/bin/windows/Rtools/
- R code to register Rtools into R (execute this in the console)

```
rtools <- "C:\\Rtools\\bin"
path <- strsplit(Sys.getenv("PATH"), ";")[[1]] 
new_path <- c(rtools, path) 
new_path <- new_path[!duplicated(tolower(new_path))] 
Sys.setenv(PATH = paste(new_path, collapse = ";"))
```
- Install required packages (copy & paste this code into R console): 


