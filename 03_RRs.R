#source("02_meta_analysis.R")


# Get RR at 95 temperature percentile -------------------------------------

list_red_95_fit <-
  # Cycle through analyses for each cause of death/age-cat combination
  pmap(list(list_model     = list_city_model,
            list_model_red = list_city_pred,
            list_pred      = list_blup_pred),
       \(list_model, list_model_red, list_pred) {
         pmap(
           # Information to get RR at 95th percentile relative to MMT for each city
           list(df_city   = list_city_df,
                model     = list_model,
                model_red = list_model_red,
                pred      = list_pred),
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
       })

# Save RR at 95th temp percentile for each city
saveRDS(list_red_95_fit, here("results", "list_red_95_fit.rds"))

# Get "average" RR for each analysis
average_95_RR <- list_red_95_fit |> 
  map(\(list_RR) {
    # RRs at 95th percentile relative to MMT and associated standard errors
    red_95_fit <- map_dbl(list_RR, \(x) x$log_RR)
    red_95_var <- map(list_RR,     \(x) x$log_RR_se^2)
    
    # Meta-regression to get intcercept term as "average" RR
    red_95_RRs_meta <- mixmeta(red_95_fit ~ 1, S = red_95_var)
    
    
    tibble(RR =  exp(coef(red_95_RRs_meta)),
           RR_lower = exp(confint(red_95_RRs_meta))[1],
           RR_upper = exp(confint(red_95_RRs_meta))[2])
  }) |> list_rbind(names_to = "analysis")

view(average_95_RR)

# Get slope for each degree increase above 95 percentile ------------------

list_red_95_99 <-
  # Cycle through analyses for each cause of death/age-cat combination
  pmap(list(list_model     = list_city_model,
            list_model_red = list_city_pred,
            list_pred      = list_blup_pred),
       \(list_model, list_model_red, list_pred) {
         pmap(
           # Information to get RR at 95th percentile relative to MMT for each city
           list(df_city   = list_city_df,
                model     = list_model,
                model_red = list_model_red,
                pred      = list_pred),
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
                  temp_diff = RR_pred$predvar - RR_pred$cen)
           })
       })

# Get "average" increase in RR for each analysis
average_95_99_RR <- list_red_95_99 |> 
  map(\(list_RR) {
    # The increase in log(RR)/temp_range and associated standard errors
    red_95_99_fit <- map_dbl(list_RR, \(x) x$log_RR/x$temp_diff)
    red_95_99_var <- map(list_RR, \(x) (x$log_RR_se/x$temp_diff)^2)
    
    # Meta-regression to get intercept term as "average" increase in RR
    red_95_99_RRs_meta <- mixmeta(red_95_99_fit ~ 1, S = red_95_99_var)
    
    
    tibble(RR =  exp(coef(red_95_99_RRs_meta)),
           RR_lower = exp(confint(red_95_99_RRs_meta))[1],
           RR_upper = exp(confint(red_95_99_RRs_meta))[2])
  }) |> list_rbind(names_to = "analysis")

view(average_95_99_RR)