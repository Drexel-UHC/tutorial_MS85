library(tidyverse)
library(glue)
library(here)
library(gt)

# Table 1 -----------------------------------------------------------------

# Read in data
df_tbl <- readRDS(here("data", "mort_temp.rds")) |> 
  # Add a copy of df with country set to "All countries" for overall calculations
  bind_rows(mutate(df, country = "All countries")) |> 
  # Order country variable and add full names for display in table
  mutate(country = factor(country,
                          levels = c("All countries", "AR", "BR", "CA", "CL", "MX", "PE"),
                          labels = c("All countries", "Argentina", "Brazil", 
                                     "Central America", "Chile", "Mexico", "Peru"))) |>
  arrange(country)

# In the following section, we create the columns of our table individually, since
# they each require unique groupings of variables to calculate. The finished columns
# are then combined and displayed with gt table. To make our table readable, we 
# use the glue() function.

# Columns 1-3: number of cities and study period for each country 
table_11 <- df_tbl |> 
  summarize(n_cities = length(unique(nsalid1)), 
            study_period = glue("{year(min(date))}-{year(max(date))}"), .by = country)

# Column 4: city population
table_12 <- df_tbl |> 
  # Our population variable changes each year, so we add a year variable
  mutate(year = year(date)) |> 
  # We extract the first observation for each country:L1:year:sex:age grouping.
  # This will allow us to sum up all of our city population observations without
  # adding repeated measurements.
  summarize(pop = first(pop), .by = c(country, nsalid1, year, sex, age)) |> 
  # We sum to get the total city population (in thousands) for each L1 and year
  summarize(city_pop = sum(pop)/1000, .by = c(country, nsalid1, year)) |> 
  # Because the population changes over the years, we take the average across all
  # years for each L1.
  summarize(city_pop = mean(city_pop), .by = c(country, nsalid1)) |> 
  # Now we extract the median, 10th and 90th percentile city average populations
  # by country
  summarize(city_pop = glue("{med} ({lower}, {upper})",
                            med   = round(median(city_pop), 0),
                            lower = round(quantile(city_pop, .1), 0),
                            upper = round(quantile(city_pop, .9), 0)), .by = country)

# Column 5: percentage aged >= 65
table_13 <- df_tbl |>
  mutate(year = year(date)) |> 
  summarize(pop = first(pop), .by = c(country, nsalid1, year, sex, age)) |> 
  mutate(age = fct_collapse(age, pop_over_65 = "65+", other_level = "pop_under_65")) |> 
  summarize(total_pop_yearly = sum(pop), .by = c(country, nsalid1, year, age)) |> 
  summarize(total_pop = mean(total_pop_yearly), .by = c(country, nsalid1, age)) |> 
  pivot_wider(names_from = age, values_from = total_pop) |> 
  mutate(pct_over_65 = 100*pop_over_65/(pop_under_65 + pop_over_65)) |> 
  summarize(pct_over_65 = glue("{med} ({lower}, {upper})",
                               med   = round(median(pct_over_65), 1),
                               lower = round(quantile(pct_over_65, .1), 1),
                               upper = round(quantile(pct_over_65, .9), 1)), .by = country)

# Column 6: annual deaths
table_14 <- df_tbl |> 
  mutate(year = year(date)) |> 
  summarize(annual_deaths = sum(deaths), .by = c(country, nsalid1, year)) |> 
  summarize(annual_deaths = mean(annual_deaths), .by = c(country, nsalid1)) |> 
  summarize(annual_deaths = glue("{med} ({lower}, {upper})",
                                 med   = round(median(annual_deaths), 0),
                                 lower = round(quantile(annual_deaths, .1), 0),
                                 upper = round(quantile(annual_deaths, .9), 0)), .by = country)

# Column 7: mean temperature
table_15 <- df_tbl |> 
  summarize(tmean = first(tmean), .by = c(country, nsalid1, date)) |> 
  summarize(tmean = mean(tmean), .by = c(country, nsalid1)) |> 
  summarize(tmean = glue("{med} ({lower}, {upper})",
                         med   = round(median(tmean), 1),
                         lower = round(quantile(tmean, .1), 1),
                         upper = round(quantile(tmean, .9), 1)), .by = country)

# Add components of our table to a list
list_table_1 <- list(table_11, table_12, table_13, table_14, table_15)

list_table_1 |> 
  # iteratively apply left_join to combine all tables by the country variable
  reduce(\(x, y) left_join(x, y, by = "country")) |> 
  # Convert our table in dataframe form to a gt table object
  gt() |> 
  # Add title
  tab_header(title = "Population, mortality, and temperature characteristics of 326 Latin American cities") |> 
  # Add footnotes
  tab_footnote(footnote = "Median (10th, 90th percentiles).", 
               location = cells_column_labels(c(city_pop, pct_over_65, annual_deaths, tmean))) |> 
  tab_footnote(footnote = "The Central America group in this anlysis consists of cities in Guatemala, Panama, Costa Rica, and El Salvador.",
               location = cells_body(columns = country, rows = country == "CA")) |> 
  tab_options(footnotes.marks = "letters") |> 
  # Rename column labels
  cols_label(country = "Countries in Latin America",
             n_cities = "No. of cities",
             study_period = "Study period",
             city_pop = "City population (thousands)",
             pct_over_65 = "Percentage aged ≥65 years",
             annual_deaths = "Annual deaths",
             tmean = "Mean temperature (°C)")

# Table 2 -----------------------------------------------------------------

# Load EDFs
df_EDF <- readRDS(here("results", "EDFs.rds"))

# Create data frame for table 2
tbl_2 <- df_EDF |> 
  mutate(death_type = 
           case_when(str_detect(analysis, "deaths") ~ "All-cause",
                     str_detect(analysis, "cardio") ~ "Cardiovascular",
                     str_detect(analysis, "respiratory") ~ "Respiratory",
                     .default = NA) |> factor(),
         age_cat = 
           ifelse(str_detect(analysis, "all_ages"),
                  "All ages (%)",
                  "Ages 65+ (%)") |> factor(levels = c("All ages (%)",
                                                       "Ages 65+ (%)"))) |> 
  pivot_wider(values_from = contains("range"), names_from = conf, names_sep = "_") |> 
  arrange(across(c(death_type, age_cat))) |> 
  mutate(across(where(is.numeric), \(x) round(x, 2))) |> 
  mutate(death_type, age_cat,
         `Total` = glue("{range_total_center} ({range_total_lower}, {range_total_upper})"),
         `All heat` = glue("{range_all_heat_center} ({range_all_heat_lower}, {range_all_heat_upper})"),
         `Extreme heat` = glue("{range_extreme_heat_center} ({range_extreme_heat_lower}, {range_extreme_heat_upper})"),
         `All cold` = glue("{range_all_cold_center} ({range_all_cold_lower}, {range_all_cold_upper})"),
         `Extreme cold` = glue("{range_extreme_cold_center} ({range_extreme_cold_lower}, {range_extreme_cold_upper})"),
         .keep = "none")

# Transform into gt table object
tbl_2 |> 
  gt(groupname_col = "death_type", rowname_col = "age_cat") |> 
  tab_header(title = "Excess death fraction associated with nonoptimal temperatures") |> 
  tab_footnote(footnote = "Percentage of total deaths explainable by temperatures above ('All heat') or below '(All cold') the city-specific optimal temperature.",
               location = cells_column_labels(Total)) |> 
  tab_footnote(footnote = "≥95th percentile of the city-specific daily temperature distribution.",
             location = cells_column_labels(`Extreme heat`)) |> 
  tab_footnote(footnote = "≤5th percentile of the city-specific daily temperature distribution.",
               location = cells_column_labels(`Extreme cold`)) |> 
  tab_options(footnotes.marks = "letters") |>
  tab_stubhead(label = "EDF") |> 
  cols_align("left")