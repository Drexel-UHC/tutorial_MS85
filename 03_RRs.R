#source("02_meta_analysis.R")

# All ages RRs ------------------------------------------------------------

list_red_95_fit <-
  pmap(
    # Information to get RR at 95th percentile relative to MMT for each city
    list(df_city   = list_city_df,
         model     = list_city_model,
         model_red = list_city_pred,
         pred      = list_blup_pred),
    # Calculation of said RR
    \(df_city, model, model_red, pred) {
      RR_pred <- 
        crosspred(model$cb_red,
                  coef = model_red$coefficients,
                  vcov = model_red$vcov,
                  model.link = model$link,
                  at = quantile(df_city$tmean, .95),
                  cen = get_MMT(pred))
      
      list(log_RR    = RR_pred$allfit,
           log_RR_se = RR_pred$allse)
         })

# Do RR of 95 percentile relative to MMT
# Slope of curve from 99th percentile from 95th percentile.

# RRs at 95th percentile relative to MMT and associated standard errors
red_95_fit <- map_dbl(list_red_95_fit, \(x) x$log_RR)
red_95_se  <- map(list_red_95_fit, \(x) x$log_RR_se)

# Meta-regression to get intcercept term as "average" RR
red_95_RRs_meta <- mixmeta(red_95_fit ~ 1, S = red_95_se)

exp(coef(red_95_RRs_meta))
exp(confint(red_95_RRs_meta))

# Overall RR increase per 1 degree C increase above 95th percentile -------
list_red_95_99 <-
  pmap(
    # Information to get RR at 99th percentile relative to 95th for each city
    list(df_city   = list_city_df,
         model     = list_city_model,
         model_red = list_city_pred,
         pred      = list_blup_pred),
    # Calculation of said RR
    \(df_city, model, model_red, pred) {
      RR_pred <- 
        crosspred(model$cb_red,
                  coef = model_red$coefficients,
                  vcov = model_red$vcov,
                  model.link = model$link,
                  at = quantile(df_city$tmean, .99),
                  cen = quantile(df_city$tmean, .95))
      
      # Estimates for increase in risk from 95th to 99th percentile
      list(log_RR     = RR_pred$allfit,
           log_RR_se  = RR_pred$allse,
           temp_range = RR_pred$predvar - RR_pred$cen)
    })

# The increase in log(RR)/temp_range and associated standard errors
red_95_99_fit <- map_dbl(list_red_95_99, \(x) x$log_RR/x$temp_range)
red_95_99_se  <- map(list_red_95_99, \(x) x$log_RR_se)

# Meta-regression to get intercept term as "average" increase in RR
red_95_99_RRs_meta <- mixmeta(red_95_99_fit ~ 1, S = red_95_99_se)

exp(coef(red_95_99_RRs_meta))
exp(confint(red_95_99_RRs_meta))
