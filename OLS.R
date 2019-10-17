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
print(cov.mat)


reg <- lm_robust(formula=wage~educ+exper+tenure, data=wage1, se_type='classical') # homoskedasticity
print(reg$vcov) # same variance-covariance matrix

print(reg_rob$vcov) #different

# cov.mat.rob <- est_robust_variance(resid, X) # doesnt work...


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
### against null: all b except for intercept = 0

R <- matrix(0, nrow=3, ncol=4)
R[1, 2] <- 1
R[2, 3] <- 1
R[3, 4] <- 1

# R <- diag(rank)
# R[1,1] <- 0
J <- rank - 1
m <- R %*% bhat
var.m <- R %*% solve(t(X)%*%X) %*% t(R)
Wald <- (t(R%*%bhat) %*% solve(var.m) %*% R%*%bhat) / sigma.hat
Fstat <- Wald/J

resid.res <- y - mean(y)
num <- (t(resid.res) %*% resid.res - t(resid) %*% resid)/J
denom <- t(resid) %*% resid/(n - rank)
Fstat2 <- num/denom

summary(reg) # same value!



##### robust variance estimation -> doesn't work...

# est_robust_variance <- function(u, X){
#   Q <- t(X) %*% X
#   sigma <- u %*% t(u)
#   P <- solve(Q) %*% t(X)
#   var <- P %*%  sigma %*% t(P)
#   return(var)
# }
# est_robust_variance(reg$residuals, X)





