source("02_meta_analysis.R")

# All Ages Attributable Fraction ------------------------------------------

list_blup_AN <-
  map(list_city_df)

attrdl(list_city_df[[1]]$tmean,
       basis = list_city_model[[1]]$cb,
       cases = list_city_df[[1]]$deaths,
       coef = list_blup_model[[1]]$blup,
       vcov = list_blup_model[[1]]$vcov,
       model.link = "log",
       type = "an",
       dir = "forw",
       cen = get_MMT(list_blup_pred[[1]]),
       sim = TRUE) |> quantile(c(.025, .975))
