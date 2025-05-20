library(tidyverse)
library(here)
library(SALURhelper)
library(dlnm); library(gnm); library(splines); library(mgcv)


# Helper functions --------------------------------------------------------

# Analyze a given city with specified parameters
analyze_city <- function(df_city, death_var = "deaths") {
  
  # Specify knots for given city
  n_lag <- 21
  pred_knots <- quantile(df_city$tmean, c(.1, .75, .9), na.rm = TRUE)
  lag_knots <- logknots(n_lag, nk = 3)
  
  # Define cross-basis
  cbt <- crossbasis(df_city$tmean,
                    lag = n_lag,
                    argvar = list(fun = "ns", knots = pred_knots),
                    arglag = list(fun = "ns", knots = lag_knots))
  
  # Define reduced cross-basis for future meta regression
  cbt_red <- onebasis(df_city$tmean, fun = "ns", knots = pred_knots)
  
  # Fit the model
  model <- gnm(deaths ~ cbt,
               family = quasipoisson(),
               eliminate = strata,
               data = df_city)
  
  list(cb = cbt,
       cb_red = cbt_red,
       pred_seq = seq(min(df_city$tmean),
                      max(df_city$tmean), 
                      length.out = 100), # Number sequence for crosspred at param
       coef = coef(model), 
       vcov = vcov(model),
       link = model$family$link)
}

# All Ages Analysis -------------------------------------------------------

# Read in data
df <- readRDS(here("data", "mort_temp.rds")) |> 
  arrange(across(c(nsalid1, date))) |> 
  # Group Central American countries together
  mutate(country = factor(ifelse(country %in% c("PA", "GT", "SV", "CR"), "CA", country))) 
  
city_names <- unique(df$nsalid1)

# Combine deaths from all ages/sexes and split data by city
list_city_df <- df |> 
  group_by(nsalid1) |> 
  group_split() |> 
  map(\(df_city) {
    df_city |> 
      summarize(across(c(deaths, respiratory, cardio), sum),
                across(c(country, tmean, pop), first), .by = c(nsalid1, date)) |> 
      mutate(strata = factor(paste(year(date), month(date), wday(date, label = TRUE), sep = ":")))
  }) |> 
  set_names(city_names)

# Analyze each city individually
list_city_model <- map(list_city_df, analyze_city)

# Get reduced prediction for each city individually
list_city_pred <- map(list_city_model, \(x) {
  pred <- crossreduce(basis = x$cb, 
                      coef  = x$coef,
                      vcov  = x$vcov,
                      model.link = x$link,
                      at = x$pred_seq)
  
  crossreduce(basis = x$cb, 
              coef  = x$coef,
              vcov  = x$vcov,
              model.link = x$link, 
              at = x$pred_seq, cen = get_MMT(pred))
  })
