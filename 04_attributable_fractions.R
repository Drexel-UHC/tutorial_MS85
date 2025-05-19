# source("02_meta_analysis.R")

# All Ages Attributable Fraction ------------------------------------------

# Total number of deaths
total_deaths <- sum(df$deaths)

# Get 1000 AN for each city
list_blup_ANs <-
  pmap(list(df_city = list_city_df,
            model = list_city_model,
            BLUP = list_blup_model,
            pred = list_blup_pred),
       \(df_city, model, BLUP, pred) {
         
         # Get AN
         AN <- 
           attrdl(df_city$tmean,
                  basis = model$cb,
                  cases = df_city$deaths,
                  coef = BLUP$blup,
                  vcov = BLUP$vcov,
                  model.link = "log",
                  type = "an",
                  dir = "forw",
                  cen = get_MMT(pred))
         
         # Get 1000 simulated ANs
         sim_ANs <- 
           attrdl(df_city$tmean,
                  basis = model$cb,
                  cases = df_city$deaths,
                  coef = BLUP$blup,
                  vcov = BLUP$vcov,
                  model.link = "log",
                  type = "an",
                  dir = "forw",
                  cen = get_MMT(pred),
                  sim = TRUE,
                  nsim = 1000)
         
         list(AN = AN,
              sim_ANs = sim_ANs)
       }) |> set_names(city_names)

# Estimate of AF
blup_AF <- sum(map_dbl(list_blup_ANs, \(x) x$AN))/total_deaths
blup_AF

# Confidence interval for AF

# A matrix of attributable numbers
# Each row is a city and each column is a simulation
matrix_blup_ANs <- map(list_blup_ANs, \(x) x$sim_ANs) |> 
  unlist() |> 
  matrix(ncol = 1000, byrow = TRUE)

blup_AF_sim <- apply(matrix_blup_ANs, 2, sum)/total_deaths

quantile(blup_AF_sim, c(.025, .975))
