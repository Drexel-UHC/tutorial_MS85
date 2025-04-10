library("tidyverse")
library("glue")
library("here")
library("dlnm"); library("gnm"); library("splines")


# Read in data ------------------------------------------------------------
df <- readRDS(here("data", "mort_temp.rds"))


list_city_df <- df |> 
  summarize(across(c(deaths, respiratory, cardio), sum), .by = c(nsalid1, date))


# Analysis all ages and <65/65+