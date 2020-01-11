### Structrual estimation of Bertran competition with common ownership

setwd('yourpath')
library(tidyverse)

### demand estimation
### create pseudo data
# set.seed(12345)
# s_jmt <- rbeta(517, 1, 75.9)
# price <- (s_jmt + rnorm(517, 0.005, 0.00016)) * 900000
# fuel_price <- price/sqrt(30000) + rnorm(517, 200, sqrt(10000))
# occupancy_rate <- rbeta(517, 9, 9/0.58-9) * 100
# weight_percentage <- rbeta(517, 9, 9/0.4-9) * 100
# mean_dist <- rbeta(517, 3, 3/0.39-3) * 2500
# mean_seat <- rbeta(517, 3, 3/0.38-3) * 500
# 
# df_pseudo <- df %>%
#   select(date, firm_code, airline_code, market) %>%
#   mutate(s_jmt=s_jmt, price=price, fuel_price=fuel_price, occupancy_rate=occupancy_rate,
#          weight_percentage=weight_percentage, mean_dist=mean_dist, mean_seat=mean_seat)


### preprocessing for pseudo data
df_pseudo <- read.csv('./data/src/pseudo_data.csv')
head(df_pseudo);summary(df_pseudo)


df_pseudo <- df_pseudo %>%
  mutate(date=as.factor(date), firm_code=as.factor(firm_code), airline_code=as.factor(airline_code)) %>% # if factor column is not identified as date-type
  group_by(airline_code, date) %>%
  mutate(s_gt=sum(s_jmt))

df_pseudo <- df_pseudo %>%
  mutate(s_0mt = 1 - s_gt, s_jmt_g = s_jmt/s_gt) # outside option share and within group share

### demand estimation(2SLS using Berry inversion)
base_demand_est <- function(df){
  base_ols <- lm(log(s_jmt)-log(s_0mt)~price+log(s_jmt_g)+date+firm_code+mean_dist+I(mean_dist^2)+I(mean_dist^3),
                 data=df)
  
  first_price <- lm(price~fuel_price+mean_seat+occupancy_rate+date+firm_code+mean_dist+I(mean_dist^2)+I(mean_dist^3),
                    data=df)
  
  first_share <- lm(log(s_jmt_g)~fuel_price+mean_seat+occupancy_rate+date+firm_code+mean_dist+I(mean_dist^2)+I(mean_dist^3),
                    data=df)
  
  df$price_hat <- first_price$fitted.values
  df$within_share_hat <- first_share$fitted.values
  
  second_stage <- lm(log(s_jmt)-log(s_0mt)~price_hat+within_share_hat+date+firm_code+mean_dist+I(mean_dist^2)+I(mean_dist^3),
                     data=df)
  return(list(base=base_ols, first_stage=list(price=first_price, share=first_share), second_stage=second_stage))
}

demand_mod <- base_demand_est(df_pseudo)
# Result of 2nd stage estimation (comparison with OLS)
summary(demand_mod$second_stage)
summary(demand_mod$base)

# Result of 1st stage estimation(check F-statistic for validity of instrumet)
summary(demand_mod$first_stage$price)
summary(demand_mod$first_stage$share)


# You need to modify the standard error(consistent estimator of \hat{\sigma}^2).
# For detail, see p410-421 of Bruce E. Hansen's Econometrics textbook available on the Internet.
calc_sigma_hat <- function(resid, weights=1, k){
  n=length(resid)
  weighted_resid <- sqrt(weights) * resid
  sigma.hat <- sqrt(t(weighted_resid) %*% weighted_resid / (n-k)) # \hat{\sigma}
  return(sigma.hat)
}


# df for computation
df_pred <- df_pseudo %>%
  mutate(price_hat=price, within_share_hat=log(s_jmt_g))
# calculate modified sigma-hat
resid_mod <- demand_mod$second_stage$model$`log(s_jmt) - log(s_0mt)` - predict(demand_mod$second_stage, df_pred)
sigma_mod <-calc_sigma_hat(resid_mod, df$weight, demand_mod$second_stage$rank)
demand_mod_res <- summary(demand_mod$second_stage)

se_mod <- sqrt(diag(demand_mod_res$cov.unscaled)) * sigma_mod # modified standard square


##### Supply
# split the data set for each period
# to avoid rank deficiency of (common) ownership matrix

df_1 <- df %>%
  filter(date == "2013-04-01")
df_2 <- df %>%
  filter(date =="2014-01-01")
df_3 <- df %>%
  filter(date == '2015-04-01')
df_4 <- df %>%
  filter(date == '2016-01-01')


### read the inverse-Omega produced in matlab to estimate the marginal cost in each model

### model1: no common ownership effect(standard Bertran competiton)
omega_inv <- as.matrix(read.csv('yourpath', header=FALSE))
df_1$mc <- df_1$price + omega_inv %*% df_1$s_jmt

omega_inv_2 <- as.matrix(read.csv('yourpath', header=FALSE))
df_2$mc <- df_2$price + omega_inv_2 %*% df_2$s_jmt

omega_inv_3 <- as.matrix(read.csv('yourpath', header=FALSE))
df_3$mc <- df_3$price + omega_inv_3 %*% df_3$s_jmt

omega_inv_4 <- as.matrix(read.csv('yourpath', header=FALSE))
df_4$mc <- df_4$price + omega_inv_4 %*% df_4$s_jmt


### model2: Bertran competition with common ownership(continuous ownership)
co_omega_inv_1 <- as.matrix(read.csv('yourpath', header=FALSE))
df_1$mc_co <- df_1$price + co_omega_inv_1 %*% df_1$s_jmt

co_omega_inv_2 <- as.matrix(read.csv('yourpath', header=FALSE))
df_2$mc_co <- df_2$price + co_omega_inv_2 %*% df_2$s_jmt

co_omega_inv_3 <- as.matrix(read.csv('yourpath', header=FALSE))
df_3$mc_co <- df_3$price + co_omega_inv_3 %*% df_3$s_jmt

co_omega_inv_4 <- as.matrix(read.csv('yourpath', header=FALSE))
df_4$mc_co <- df_4$price + co_omega_inv_4 %*% df_4$s_jmt


df_mc <- rbind(df_1, df_2, df_3, df_4)


res_co <- lm(mc_co~cum_travelers_num+cum_distance+I(cum_distance^2)+cum_number+occupancy_rate+weight_percentage+date+name+date:name+date:airline_name,
             data=df_mc)

res_noco <- lm(mc~cum_travelers_num+cum_distance+I(cum_distance^2)+cum_number+occupancy_rate+weight_percentage+date+name+date:name+date:airline_name,
               data=df_mc)
summary(res_co)
summary(res_noco)


##### Vuong's Test
### See Rivers and Vuong(2002) for the construction of this statistic
perform_vuong_test <- function(res_1, res_2){
  resid_1 <- resid(res_1)
  resid_2 <- resid(res_2)
  n = length(resid_1)
  
  m_1 <- 2 * t(resid_1)
  m_2 <- 2 * t(resid_2)
  
  Sigma_1 <- resid_1 %*% t(resid_1)
  Sigma_2 <- resid_2 %*% t(resid_2)
  
  
  sigma_hat <- m_1 %*% Sigma_1 %*% t(m_1) - m_2 %*% Sigma_2 %*% t(m_2)
  if (sigma_hat < 0) {
    vuong_stat <-  - sqrt(n/abs(sigma_hat)) * (crossprod(resid_2) - crossprod(resid_1))
  }
  else{
    vuong_stat <- sqrt(n/sigma_hat) * (crossprod(resid_1) - crossprod(resid_2))
  }
  return(list(v_stat=vuong_stat, p_val=pnorm(vuong_stat), variance=sigma_hat))
}

vuong_test_res <- perform_vuong_test(res_co, res_noco)
vuong_test_res
