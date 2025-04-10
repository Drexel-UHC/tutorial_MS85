library("tidyverse")
library("here")

# Read metadata describing cities -----------------------------------------

# Paths to read locations
path_overall     <- "//files.drexel.edu/encrypted/SOPH/UHC/Projects/SALURBAL HEAT Study/"
path_metadata    <- paste0(path_overall,"Data/Files/Derived Data/Meta Data/")
path_climatedata <- paste0(path_overall,"Data/Files/Climate Zone/Processed Data/")
path_deriveddata <- paste0(path_overall,"Data/Files/Derived Data/")
path_all_cause   <- paste0(path_deriveddata, "All Cause/")
path_respiratory <- paste0(path_deriveddata, "Respiratory/")
path_cardio      <- paste0(path_deriveddata, "Cardiovascular/")

# Read metadata
metadata0 <- haven::read_sas(paste0(path_metadata, "city_level_temp.sas7bdat"))
hasmortality <- haven::read_sas(paste0(path_metadata, "hasmortality_indic.sas7bdat"))
clusters <- read_csv(paste0(path_overall, "Analysis/Clusters/Results/ECDFClusters.csv"))
climate_labels <- read_csv(paste0(path_climatedata, "climate_labels2.csv"))
inAICcalculations <- read_csv(paste0(path_metadata, "inAICcalculations.csv"), col_select = c(nsalid1, inAIC))

# Aggregate metadata
metadata <-
  metadata0 |> 
  left_join(hasmortality, by = "nsalid1") |> 
  left_join(inAICcalculations, by = "nsalid1") |> 
  left_join(climate_labels, by = "KP_climate_zone") |> 
  left_join(clusters, by = "nsalid1") |> 
  #left_join(citycodes, by = join_by(nsalid1 == SALID1)) |> 
  filter(hasMortality == 1,
         inAIC == 1) |> 
  mutate(KP_Precip_Interpret = as_factor(KP_Precip_Interpret),
         filename = paste0("c", nsalid1, ".sas7bdat")) |> 
  arrange(nsalid1)

# Save aggregated metadata
write_csv(metadata, here("data", "metadata.csv"))
saveRDS(metadata,   here("data", "metadata.rds"))

# Read city specific data -------------------------------------------------

# Raw file locations of data
list_datafiles <- 
  list(all_cause = path_all_cause,     # All cause mortality
       respiratory = path_respiratory, # Respiratory deaths
       cardio = path_cardio) |>        # Cardiovascular deaths
  map(\(p) paste0(p, "/c", metadata$nsalid1, ".sas7bdat")) # Files for each city

# Function to read in sas7bdat file, convert to dataframe, and format
process_data <- function(path_to_data) {
  path_to_data |>
    haven::read_sas() |>
    mutate(country = toupper(country), 
           nsalid1 = as.factor(nsalid1),
           date = ymd(allDate),
           sex  = factor(male, levels = c(0, 1), labels = c("Female", "Male")),
           age = factor(thisage, levels = c("Infant", "1-4", "5-19", "20-34", "35-49",  "50-64", "65+")),
           deaths = deaths,
           tmean = ADtemp_pw,
           logpop = log(pop_count), .keep = "none") |>
    arrange(across(c(country, nsalid1, date, sex, age))) |> # Order rows
    filter(!is.na(tmean)) # Remove days with missing temperature
}

# Create dataframe  each city and for each mortality type and order columns
df <- 
  pmap(list_datafiles,
       \(all_cause, respiratory, cardio) {
         # Variables that uniquely specify a row
         each_row <- c("nsalid1", "date", "sex", "age")
         
         # Process data for all cause, respiratory, and cardiovascular deaths
         df1 <- map(all_cause, process_data)   |> list_rbind()
         df2 <- map(respiratory, process_data) |> list_rbind() |> select(all_of(each_row), respiratory = deaths)
         df3 <- map(cardio, process_data)      |> list_rbind() |> select(all_of(each_row), cardio = deaths)
         
         # Return a single dataframe with 3 death columns:
         # all cause, respiratory, and cardio
         df1 |> 
           left_join(df2, by = each_row, keep = FALSE) |> 
           left_join(df3, by = each_row, keep = FALSE)
           # mutate(resp = df2$deaths,
           #        cardio = df3$deaths, .before = tmean)
       }) |> list_rbind() |> 
  select(c(country, nsalid1, date, sex, age, # Unique determinant of each row
           deaths, respiratory, cardio,      # Deaths by type
           tmean, logpop)) # Pop weighted mean daily temperature and log pop
df_brute

# Write multi city object to disk
write_csv(df, here("data", "mort_temp.csv"))
saveRDS(df,   here("data", "mort_temp.rds"))


# Read city specific details ----------------------------------------------



# Other stuff -------------------------------------------------------------

df_test[[1]] |> glimpse()

test_derek |> 
  mutate(death = deaths, 
         tmean = ADtemp_pw,
         logpop = pop_count/1000,
         date = lubridate::ymd(allDate),
         dow = lubridate::wday(date, label = TRUE, abbr = FALSE),
         year = lubridate::year(date)) |> 
  select(all_of(varkeep)) |> 
  filter(!is.na(tmean))


test_derek <- 
  read_my_data(list_datafiles[[1]]) |> 
  mutate(maletxt = factor(male, levels = c(0, 1), labels = c("Female", "Male")),
         thisage = factor(thisage, levels = c("Infant", "1-4", "5-19", "20-34", "35-49",  "50-64", "65+")),
         group_series = thisage:maletxt)

test_derek |>
  select(all_of(c("nsalid1", "tmean", "date"))) |> 
  unique()

test_derek |> 
  summarize(totaldeaths = sum(death),
            years_of_data = length(unique(year)),
            AvPop_in1000s = sum(exp(logpop))/(365.25*years_of_data))

test_derek |> 
  summarize(death = sum(death),
            Pop.in1000s = sum(exp(logpop)/(365.28*length(unique(test_derek$year)))), .by = thisage)

quantile(test_derek$tmean, prob = c(0, 0.01, 0.025, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.975, 0.99, 1))

test_brisa <- read.my.data(list_datafiles[[1]])

varkeep= c("nsalid1","death","tmean","dow","date","year","thisage","male","logpop")
varfun = "ns"  # using natural cubic spline
vardegree = 3  # not needed when varfun="ns" (already cubic. if we switched to "bs" then specify vardegree=2)

#FUNCTION TO READ IN SAS DATASET
read.my.data = function(datafilename, subset=FALSE, strata=NULL){ lllll
  #values for strata should be in quotes, like: "thisage=='65+' & male==1"
  foo=read.sas7bdat(datafilename) 
  foo$death = foo$deaths
  foo$tmean = foo$ADtemp_pw
  foo$logpop = log(foo$pop_count/1000) #need to verify if population counts are in # of people or 1000's of people
  foo$date= as.Date(foo$allDate, origin="1960-01-01")
  foo$dow=weekdays(foo$date)
  foo$year=format(foo$date,"%Y") 
  
  # keep only the rows we need
  if(subset==TRUE){ foo = subset(foo, return(eval(parse(text=strata)))) }
  
  # keep only the variables we need
  foo = subset(foo, is.na(tmean)==FALSE , select=varkeep )  
  
  #return:
  foo
}

read_my_data <- function(filename, subset = FALSE, strata = NULL) {
  filename |> 
    haven::read_sas() |> 
    mutate(death = deaths, 
           tmean = ADtemp_pw,
           logpop = pop_count/1000,
           date = lubridate::ymd(allDate),
           dow = lubridate::wday(date, label = TRUE, abbr = FALSE),
           year = lubridate::year(date)) |> 
    select(all_of(varkeep)) |> 
    filter(!is.na(tmean))
}

ggplot(test_derek, aes(date, tmean, color = male)) +
  geom_point()


test_derek$thisage |> factor(levels = c("Infant", "1-4", "5-19", "20-34", "35-49",  "50-64", "65+"))
