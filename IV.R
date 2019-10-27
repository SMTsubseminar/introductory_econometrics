# setwd('your path')
### endogeneity and instrumental variable

# install.packages("AER")
# install.packages("stargazer")

library(AER);library(stargazer);library(wooldridge);library(dplyr)

###### Sps we want to est return of education.
data(card);head(card);help(card)
summary(card)

card <- as.data.frame(card)

head(card)


# apply(is.na(card), MARGIN=2, sum)
# card.cld <- na.omit(card)
# apply(is.na(card.cld), MARGIN=2, sum)

### IV by package

## ols first(baseline)
# select columns(regressors)
usecols <- cols[c(4:7, 9:24, 32, 34)]
ols.formula <- as.formula(paste("lwage~", paste(usecols, collapse="+")))
str(ols.formula)

make_formula <- function(columns, index, y="lwage~"){
  usecols <- columns[index]
  formula <- as.formula(paste(y, paste(usecols, collapse="+")))
  return(formula)
}

ols <- lm(ols.formula, data=card.cld)
summary(ols) # something wrong→why?

# dummies
# why expr singularity?

### how to solve this problem?

###################################

# my solution
new.index <- c(4, 5, 13:20, 22:23, 34)
cols[new.index]
new.forml <- make_formula(cols, new.index)
ols <- lm(new.forml, data=card)
summary(ols)
# return of education is negative at 5% significance level!


### verify by instrumental variable regression
## relevance check
cols[c(2, 3)]
rel.formula <- make_formula(cols, c(2, 3, 5, 13:20, 22:23, 34), y='educ~')
rel.check <- lm(rel.formula, data=card)
summary(rel.check) 


make_ivformula <- function(columns, endo, exog, y="lwage~"){
  endo.cols <- columns[endo]
  exog.cols <- columns[c(endo[2:length(endo)], exog)]
  RHS <- paste(paste(endo.cols, collapse="+"), sep="|", paste(exog.cols, collapse="+"))
  formula <- as.formula(paste(y, RHS))
  return(formula)
}


iv.formula <- make_ivformula(cols, new.index, c(2, 3))

reg.iv <- ivreg(formula=iv.formula, data=card)
summary(reg.iv) # not significant...


### how to use stargazer
stargazer(rel.check, ols, reg.iv, type='text', keep=c("educ", "nearc4", "age", "black"),
          keep.stat=c("n"))

stargazer(rel.check, ols, reg.iv, type='latex', keep=c("educ", "nearc4", "age", "black"),
          keep.stat=c("n"))


#### Implementation by hand!!




