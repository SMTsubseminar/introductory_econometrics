setwd('your path')

##### OLS

install.packages("estimatr")
library(estimatr)
library(wooldridge)
library(dplyr)


##### implementation using lm class

data("wage1")
help("wage1")
head(wage1)

reg <- lm(formula=wage~educ+exper+tenure, data=wage1)
summary(reg)
plot(reg)

### You can obtain the robust standard errors using estimatr
reg_rob <- lm_robust(formula=wage~educ+exper+tenure, data=wage1)
summary(reg_rob) # significance level changed!
reg_rob$vcov

plot(reg_rob)


##### implementation by matrix form
### independent variables
X <- wage1 %>%
  mutate(intercept=1) %>%
  select(intercept, educ, exper, tenure) %>%
  as.matrix()

print(X[1:3,])

y <- wage1[, 'wage']

est_beta <- function(X, y){
  Q <- t(X) %*% X
  bhat <- solve(Q) %*% t(X) %*% y
  return(bhat)
}

bhat <- est_beta(X, y)
print(bhat)
print(reg_rob$coefficients) # same result

rank <- ncol(X)
n <- length(y)

fitted.val <- X %*% bhat
resid <-  y - fitted.val


sigma.hat <- as.numeric((t(resid) %*% resid)/(n - rank))
cov.mat <- sigma.hat * solve(t(X) %*% X)
print(covario.mat)


reg <- lm_robust(formula=wage~educ+exper+tenure, data=wage1, se_type='classical') # homoskedasticity
print(reg$vcov) # same variance-covariance matrix

print(reg_rob$vcov) #different

cov.mat.rob <- est_robust_variance(resid, X) # doesnt work...


fitting <- function(y, X, bhat){
  fitted <- X %*% bhat
  resid <- y - fitted
  return(list(fitted=fitted, resid=resid))
}

res <- fitting(y, X, bhat)
head(res$fitted)
head(res$resid)


##### inference


### t-test under homoskedasticity
### against null: b = 0
se <- sqrt(diag(cov.mat))
tstat <- bhat/se
print(tstat)


### F-test under homoskedasticity(linear restriction)
### against null: all b = 0



##### MLE estimation of OLS



##### GMM estimation of OLS





##### robust variance estimation -> doesn't work...

# est_robust_variance <- function(u, X){
#   Q <- t(X) %*% X
#   sigma <- u %*% t(u)
#   P <- solve(Q) %*% t(X)
#   var <- P %*%  sigma %*% t(P)
#   return(var)
# }
# est_robust_variance(reg$residuals, X)





