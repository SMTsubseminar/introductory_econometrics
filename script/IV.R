# setwd('your path')
### endogeneity and instrumental variable

# install.packages("AER")
# install.packages("stargazer")

library(AER);library(stargazer);library(wooldridge);library(dplyr)

###### Sps we want to est return of education.
data(card);head(card);help(card)
summary(card)


###### cleansing/preprocessing
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

proj_P <- function(X, Z, Y){
  projecter <- solve(t(Z) %*% X) %*% t(Z)
  return(projecter %*% Y)
}



est_beta <- function(X, y){
  bhat <- proj_P(Z, Z, y)
  return(bhat)
}


fitting <- function(y, X, bhat){
  fitted <- X %*% bhat
  resid <- y - fitted
  return(list(fitted=fitted, resid=resid))
}

card.blck <- card %>%
  filter(black==1) %>%
  as.data.frame()

summary(card.blck)

X <- card.blck[c('educ', 'exper', 'expersq', 'reg662', 'reg663', 'reg664',
                 'reg665', 'reg666', 'reg667', 'reg668', 'reg669')] %>%
  as.matrix()
Z <- card.blck[c('nearc4', 'exper', 'expersq', 'reg662', 'reg663', 'reg664',
                 'reg665', 'reg666', 'reg667', 'reg668', 'reg669')] %>%
  as.matrix()

# just identified

y <- card.blck[, 'lwage'] %>%
  as.matrix()
n <- length(y)

iv_est <- function(Z, X, y){
  n <- length(y)

  b.iv <- proj_P(Z, X, y)
  fit.res <- fitting(y, X, b.iv)

  QZ <- t(Z) %*% Z
  QX <- t(Z) %*% X
  sigma.hat <- as.numeric(t(fit.res$resid) %*% fit.res$resid / n)
  est.cov <- sigma.hat * solve(QX) %*% QZ %*% t(solve(t(X)%*%Z))
  return(list(b.iv=b.iv, fitted=fit.res$fitted, resid=fit.res$resid, est.cov=est.cov, sigma.hat2=sigma.hat))
}

res <- iv_est(Z, X, y)
res$b.iv
head(res$resid)
se <- diag(res$est.cov)
print(se)

tstat <- res$b.iv[1]/se[1] # not significant

# 95%CI
c(res$b.iv[1] -1.96 * se[1], res$b.iv[1] + 1.96 * se[1])




### Two stage least squares
# do_tsls <- function(X, Z, y){
#   X.hat <-Z %*% proj_P(Z, X)
#   resid.X <- X - X.hat
#   b.iv <- proj_P(Z, y)
#   resid.y <- y - X.hat %*% b.iv
#   return(list(Xhat=X.hat, resid1=resid.X, b.iv=b.iv, resid2=resid.y))
# }
# res <- do_tsls(X, Z, y)
# 
# res$b.iv
