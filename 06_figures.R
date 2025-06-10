# General Libraries
library(tidyverse)
library(here)
library(patchwork)
library(lubridate)
library(glue)

# ggplot2 theming
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             strip.background = element_blank(),
             strip.text = element_text(size = 12),
             axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))


# Figure 1 ----------------------------------------------------------------

# Read data for temperature
df <- readRDS(here("data", "mort_temp.rds")) 

# Read shape files
df_l1_sf <- read_sf("//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/Wellcome_Trust/Data Methods Core/Geodatabases/SALURBAL/Shapefiles/SALURBAL_L1.shp") |> mutate(SALID1 = factor(SALID1))
df_la_sf <- read_sf(here("data", "LA_outline", "LA_outline.shp"))

# Calculate annual mean temperatures
df_l1_temp <- df |> 
  # Add year variable for grouping
  mutate(year = year(date)) |> 
  # Extract each all temperature without duplicates
  summarize(tmean = first(tmean), .by = c(country, nsalid1, year, date)) |> 
  # Get yearly mean temperature for each L1
  summarize(tmean = mean(tmean), .by = c(country, nsalid1, year)) |> 
  # Get average yearly mean temperature for each L1
  summarize(tmean = mean(tmean), .by = c(country, nsalid1))

# Combine shape and temperature information
df_l1 <- right_join(df_l1_sf, df_l1_temp, by = join_by(SALID1 == nsalid1))

# Create ggplot object
ggplot() +
  # Plot country outlines
  geom_sf(data = df_la_sf) +
  # Plot city temperatures
  geom_sf(aes(fill=tmean), shape = 21, data = df_l1) +
  # Specify the x & y coordinates of the plot
  coord_sf(xlim = c(-120, -38), ylim = c(-55, 30)) +
  # Select the color scheme
  scale_fill_viridis_c(option = "plasma") +
  # Add labels
  labs(fill = "Temp. (°C)", x = "Longitude", y = "Latitude") +
  # Minor adjustments to the legend
  theme(legend.position = c(.1, .16),
        legend.background = element_rect(color = "grey"),
        legend.title = element_text(size = 6),
        legend.key.height = unit(.15, 'in'),
        legend.key.width = unit(.15, 'in'))

# Save the figure 1 as a png file
ggsave("results/figure_1.png", width = 4, height = 9, units = 'in')

# Figure 2 ----------------------------------------------------------------
fig_2_cities <- c("101112", "102190", "204191", "105113", "204141", "103116")

fig_2_plots <- map(fig_2_cities, \(ID) {
  list_blup_pred[[ID]] |> 
    get_df_RRs() |> 
    rename(Temperature = exposure) |> 
    ggplot(aes(x = Temperature)) +
    geom_line(aes(y = RR)) +
    geom_ribbon(aes(ymin = RR_low, ymax = RR_high), alpha = .2) + 
    geom_hline(yintercept = 1) +
    scale_y_continuous(limits = c(.75, 3.5),
                       breaks = c(0.8, 1, 1.25, 1.5, 2),
                       transform = "log") +
    labs(title = ID)
  })

wrap_plots(fig_2_plots)

# Figure 3 ----------------------------------------------------------------


# Extended Data: Figure 1 -------------------------------------------------


# Extended Data: Figure 2 -------------------------------------------------


