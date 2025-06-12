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
                         at = quantile(df_city$tmean, c(0.05, .95)),
                         cen = get_MMT(pred))
             
             tibble(log_RR     = RR_pred$allfit,
                    log_RR_var = RR_pred$allse^2,
                    exposure = c('cold', 'heat'))
           }) |> list_rbind(names_to = "nsalid1")
       }) 

# Get "average" RR for each analysis
average_95_RR <- list_red_95_fit |> 
  map(\(df_RR) {
    # Meta-regression to get intercept term as average RR
    red_95_heat <- mixmeta(log_RR ~ 1, S = log_RR_var, data = df_RR, subset = (exposure == "heat"))
    red_95_cold <- mixmeta(log_RR ~ 1, S = log_RR_var, data = df_RR, subset = (exposure == "cold"))
    
    tibble(RR       = exp(c(coef(red_95_cold), coef(red_95_heat))),
           RR_lower = exp(c(confint(red_95_cold)[1], confint(red_95_heat)[1])),
           RR_upper = exp(c(confint(red_95_cold)[2], confint(red_95_heat)[2])),
           exposure = c("cold", "heat"))
  }) |> list_rbind(names_to = "analysis")

average_95_RR |> 
  filter(analysis == "all_ages_deaths") |> 
  view()

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
             RR_heat <- 
               crosspred(model$cb_red,
                         coef = model_red$coefficients,
                         vcov = model_red$vcov,
                         model.link = model$link,
                         at = quantile(df_city$tmean, .99),
                         cen = quantile(df_city$tmean, .95))
             
             RR_cold <- 
               crosspred(model$cb_red,
                         coef = model_red$coefficients,
                         vcov = model_red$vcov,
                         model.link = model$link,
                         at = quantile(df_city$tmean, .01),
                         cen = quantile(df_city$tmean, .05))
             
             # Estimates for increase in risk from 95th to 99th percentile
             tibble(temp_diff  = abs(c(RR_cold$predvar - RR_cold$cen, RR_heat$predvar - RR_heat$cen)),
                    log_RR     = c(RR_cold$allfit/temp_diff[1], RR_heat$allfit/temp_diff[2]),
                    log_RR_var = c((RR_cold$allse/temp_diff[1])^2, (RR_heat$allse/temp_diff[2])^2),
                    exposure = c("cold", "heat"))
           }) |> list_rbind(names_to = "nsalid1")
       })

# Get "average" increase in RR for each analysis
average_95_99_RR <- list_red_95_99 |> 
  map(\(df_RR) {
    # Meta-regression to get intercept term as average increase in RR
    red_95_99_heat <- mixmeta(log_RR ~ 1, S = log_RR_var, data = df_RR, subset = (exposure == "heat"))
    red_95_99_cold <- mixmeta(log_RR ~ 1, S = log_RR_var, data = df_RR, subset = (exposure == "cold"))
    
    # Get average increase in RR per degree change in temperature
    tibble(RR       = exp(c(coef(red_95_99_cold), coef(red_95_99_heat))),
           RR_lower = exp(c(confint(red_95_99_cold)[1], confint(red_95_99_heat)[1])),
           RR_upper = exp(c(confint(red_95_99_cold)[2], confint(red_95_99_heat)[2])),
           exposure = c("cold", "heat"))
    
  }) |> list_rbind(names_to = "analysis")

average_95_99_RR |> 
  filter(analysis == "all_ages_deaths") |> 
  view()
