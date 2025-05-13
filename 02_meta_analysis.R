#source("01_city_specific_models.R")
library(mixmeta)

# All Ages Meta-Regression ------------------------------------------------
# Predictors by city
df_meta <- df |> 
  summarize(p50 = median(tmean),
            range_high = max(tmean) - p50,
            range_low  = min(tmean) - p50,
            country = first(country), .by = nsalid1)

# Coefficients by city (response of meta regression)
mat_coef <- map_dfr(list_city_pred, \(x) x$coefficients) |> as.matrix()

# Variance-covariance matrices by city model (within city variance)
list_vcov <- map(list_city_pred, \(x) x$vcov)

# Meta-regression model
model_meta <- mixmeta(mat_coef ~ 
                        ns(p50, df = 2) +
                        ns(range_high, df = 2) +
                        ns(range_low, df = 2),
                        # country, Adding 
                      S = list_vcov,
                      data = df_meta)

# Best linear unbiased predictors of coefs for each city
list_blup_model <- blup(model_meta, vcov = TRUE) |> set_names(city_names)

# Reduced prediction via meta-regression for each city
list_blup_pred <- map2(list_city_model, list_blup_model, \(model, BLUP) {
  pred <- crosspred(basis = model$cb_red,
                    coef  = BLUP$blup,
                    vcov  = BLUP$vcov,
                    model.link = model$link,
                    at = model$pred_seq)
  
  crosspred(basis = model$cb_red, 
            coef  = BLUP$blup,
            vcov  = BLUP$vcov,
            model.link = model$link, 
            at = model$pred_seq, cen = get_MMT(pred))
})
