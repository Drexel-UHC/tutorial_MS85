library("tidyverse")
library("here")
library("dlnm"); library("gnm"); library("splines")


# Helper functions --------------------------------------------------------
# Get MMT from output of crosspred or crossreduce
get_cen <- function(crosspred) {
  # We only need this if statement because the output of crosspred and 
  # crossreduce is slightly different. We want this function to work with both.
  if(!is.null(crosspred$fit)) {
    crosspred$predvar[which.min(crosspred$fit)]
  } else if(!is.null(crosspred$allfit)) {
    crosspred$predvar[which.min(crosspred$allfit)]
  } else {
    print("Unable to locate minimum.")
  }
}

# Analyze a given city with specified parameters
analyze_city <- function(df_city, death_var = deaths) {
  # Specify knots for given city
  n_lag <- 21
  pred_knots <- quantile(df_city$tmean, c(.1, .75, .9), na.rm = TRUE)
  lag_knots <- logknots(n_lag, nk = 3)
  
  # Define cross-basis
  cbt <- crossbasis(df_city$tmean,
                    lag = n_lag,
                    argvar = list(fun = "ns", knots = pred_knots),
                    arglag = list(fun = "ns", knots = lag_knots))
  
  # Fit the model
  model <- gnm(death ~ cbt,
               family = quasipoisson(),
               eliminate = strata,
               data = df_city)
}



# All Ages Analysis -------------------------------------------------------

# Read in data
df <- readRDS(here("data", "mort_temp.rds"))



# Combine deaths from all ages/sexes and split data by city
list_city_df <- df |> 
  summarize(across(c(deaths, respiratory, cardio), sum),
            across(c(country, tmean, pop), first), .by = c(nsalid1, date))

# Analyze each city individually



# Analysis all ages and <65/65+