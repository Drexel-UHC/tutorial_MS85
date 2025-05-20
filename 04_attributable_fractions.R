# source("02_meta_analysis.R")

# All Ages Attributable Fraction ------------------------------------------

# Total number of deaths
total_deaths <- sum(df$deaths)

# Get attributable numbers for each city
list_blup_ANs <-
  pmap(list(df_city = list_city_df,
            model = list_city_model,
            BLUP = list_blup_model,
            pred = list_blup_pred),
       \(df_city, model, BLUP, pred) {
         
         # Specify temperature ranges of interest
         temp_ranges <- list(
           range_total        = range(df_city$tmean),
           range_all_heat     = c(get_MMT(pred), max(df_city$tmean)),
           range_extreme_heat = quantile(df_city$tmean, c(.95, 1)),
           range_all_cold     = c(min(df_city$tmean), get_MMT(pred)),
           range_extreme_cold = quantile(df_city$tmean, c(0, .05))
         )
         
         temp_ranges |> 
           map(\(range) {
             # Get AN (for EDF estimate)
             AN <- attrdl(df_city$tmean,
                          basis = model$cb,
                          cases = df_city$deaths,
                          coef = BLUP$blup,
                          vcov = BLUP$vcov,
                          model.link = "log",
                          type = "an",
                          dir = "forw",
                          cen = get_MMT(pred),
                          range = range)
             
             # Get 1000 simulated ANs (for lower/upper bounds of EDF estimate)
             sim_ANs <- attrdl(df_city$tmean,
                               basis = model$cb,
                               cases = df_city$deaths,
                               coef = BLUP$blup,
                               vcov = BLUP$vcov,
                               model.link = "log",
                               type = "an",
                               dir = "forw",
                               cen = get_MMT(pred),
                               range = range,
                               sim = TRUE,
                               nsim = 1000)
             
             list(AN = AN,
                  sim_ANs = sim_ANs)
           })
       }) |> set_names(city_names)

# Estimate of AF

list_blup_ANs |> 
  map(\(city) {
    city |> 
      map(\(temp_range) {
        # Sum of total deaths attributable to a given non-optimal temperature range
        blup_AF <- sum(map_dbl(temp_range$AN))/total_deaths
        
        matrix_blup_ANs <- temp_range$sim_ANs |> 
          unlist() |> 
          matrix(ncol = 1000, byrow = TRUE)
      })
  })

# Attributable fraction for each city and temperature range
df_blup_AN <- list_blup_ANs |> 
  map(\(city) {
    city |> 
      map_dbl(\(temp_range) {
        temp_range$AN
      }) |> as_tibble_row()
  }) |> list_rbind(names_to = "nsalid1")

df_AF <- df_blup_AN |> 
  summarize(across(contains("range"), \(x) 100*sum(x)/total_deaths))

# Confidence interval for each city and temperature range
df_blup_sim_ANs <- list_blup_ANs |> 
  map(\(city) {
    city |> 
      map(\(temp_range) {
        temp_range$sim_ANs
      }) |> as_tibble() |> mutate(iter = 1:1000, .before = range_total)
  }) |> list_rbind(names_to = "nsalid1")

df_AF_conf <- df_blup_sim_ANs |> 
  summarize(across(contains("range"), \(x) 100*sum(x)/total_deaths), .by = iter) |> 
  reframe(conf = c("lower", "upper"),
          across(contains("range"), \(x) quantile(x, c(.025, .975))))

# Final estimate
df_EDF <- 
  mutate(df_AF, conf = "center", .before = range_total) |> 
  bind_rows(df_AF_conf) |> 
  mutate(cause = "all_cause", age = "all_ages", .before = "conf")

saveRDS(df_EDF, here("data", "EDFs.rds"))
