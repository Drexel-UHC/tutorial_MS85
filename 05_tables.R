library(tidyverse)
library(glue)
library(here)
library(gt)

# Table 1 -----------------------------------------------------------------

# Read in data
df <- readRDS(here("data", "mort_temp.rds")) |> 
  arrange(across(c(nsalid1, date))) |> 
  # Combine Central American countries
  mutate(country = factor(ifelse(country %in% c("PA", "GT", "SV", "CR"), "CA", country))) |> 
  # Add a copy of df with country set to "All countries" for overall calculations
  bind_rows(mutate(df, country = "All countries")) |> 
  # Order country variable
  mutate(country = factor(country, levels = c("All countries", "AR", "BR", "CA", "CL", "MX", "PE"))) |> 
  arrange(country)

# Number of cities, study period
table_11 <- df |> 
  summarize(n_cities = length(unique(nsalid1)), 
            study_period = glue("{year(min(date))}-{year(max(date))}"), .by = country)

# City population
table_12 <- df |> 
  summarize(pop = first(pop), .by = c(country, nsalid1, sex, age)) |> 
  summarize(city_pop = sum(pop)/1000, .by = c(country, nsalid1)) |> 
  summarize(city_pop = glue("{med} ({lower}, {upper})",
                            med   = round(median(city_pop), 0),
                            lower = round(quantile(city_pop, .1), 0),
                            upper = round(quantile(city_pop, .9), 0)), .by = country)

# Percentage aged >= 65
table_13 <- df |>
  summarize(pop = first(pop), .by = c(country, nsalid1, sex, age)) |> 
  mutate(age = fct_collapse(age, pop_over_65 = "65+", other_level = "pop_under_65")) |> 
  summarize(total_pop = sum(pop), .by = c(country, nsalid1, age)) |> 
  pivot_wider(names_from = age, values_from = total_pop) |> 
  mutate(pct_over_65 = 100*pop_over_65/(pop_under_65 + pop_over_65)) |> 
  summarize(pct_over_65 = glue("{med} ({lower}, {upper})",
                               med   = round(median(pct_over_65), 1),
                               lower = round(quantile(pct_over_65, .1), 1),
                               upper = round(quantile(pct_over_65, .9), 1)), .by = country)

# Annual deaths
table_14 <- df |> 
  mutate(year = year(date)) |> 
  summarize(annual_deaths = sum(deaths), .by = c(country, nsalid1, year)) |> 
  summarize(annual_deaths = glue("{med} ({lower}, {upper})",
                                 med   = round(median(annual_deaths), 0),
                                 lower = round(quantile(annual_deaths, .1), 0),
                                 upper = round(quantile(annual_deaths, .9), 0)), .by = country)

# Mean temperature
table_15 <- df |> 
  summarize(tmean = first(tmean), .by = c(country, nsalid1, date)) |> 
  summarize(tmean = glue("{med} ({lower}, {upper})",
                         med   = round(median(tmean), 1),
                         lower = round(quantile(tmean, .1), 1),
                         upper = round(quantile(tmean, .9), 1)), .by = country)

# Combine all table
list_table_1 <- list(table_11, table_12, table_13, table_14, table_15)

list_table_1 |> 
  reduce(\(x, y) left_join(x, y, by = "country")) |> 
  gt() |> 
  tab_header(title = "Population, mortality, and temperature characteristics of 326 Latin American cities") |> 
  tab_footnote(footnote = "Median (10th, 90th percentiles).", 
               location = cells_column_labels(c(city_pop, pct_over_65, annual_deaths, tmean))) |> 
  tab_footnote(footnote = "The Central America group in this anlysis consists of cities in Guatemala, Panama, Costa Rica, and El Salvador.",
               location = cells_body(columns = country, rows = country == "CA")) |> 
  tab_options(footnotes.marks = "letters") |> 
  cols_label(country = "Countries in Latin America",
             n_cities = "No. of cities",
             study_period = "Study period",
             city_pop = "City population (thousands)",
             pct_over_65 = "Percentage aged ≥65 years",
             annual_deaths = "Annual deaths",
             tmean = "Mean temperature (°C)")

# Table 2 -----------------------------------------------------------------


