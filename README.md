# Extreme temperatures and mortality in 326 Latin American cities: a longitudinal ecological study

This folder contains all the code files needed to reproduce the findings in the paper. 
The files cannot be run without the mortality data, however. The mortality data cannot be made available 
due to data use agreements with the various countries.

# Files

- `00_read_data.R`: Reads in and formats the data on the Drexel servers. Creates two dataframes, `mort_temp.rds`, for use in the city-specific analysis, and `metadata.rds`, for use in the meta-analysis.
- `01_city_specific_models.R`: Runs the analysis for each city and cause of death. The resulting coefficients are used in the meta-analysis.
- `02_meta_analysis.R`: Runs the meta-analysis on the coefficients of all cities. The resulting curves are used when estimating the risk ratios and attributable fractions.
- `03_RRs.R`: Calculated the risk ratios at the 5th and 95th temperature percentile centered at the MMT. Also calculates the increase in RR per increase in 1° C of extreme heat and decrease for extreme cold.
- `04_attributable_fractions.R`: Calculates the attributable fractions for exposure non-optimal temperature, heat, cold, extreme heat, and extreme cold.
- `05_tables.R`: Recreates all tables in [MS85](https://doi.org/10.1038/s41591-022-01872-6).
- `06_figures.R`: Recreates all figures in [MS85](https://doi.org/10.1038/s41591-022-01872-6).

# Additional Resources

Additional city-specific results and summary information can be found in an interactive web application here: https://drexel-uhc.shinyapps.io/MS85/
   
The data and code repository for estimating daily temperatures is here: https://github.com/Drexel-UHC/salurbal_heat

All the code used here is heavily based on the code used for the analysis in the paper "Mortality risk attributable to high and low ambient temperature: a multi-country study" by Antonio Gasparrini and collaborators (The Lancet, 2015)

We downloaded their code from here: https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata and edited it as needed for our analysis.