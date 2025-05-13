library(patchwork)

# ggplot2 theming
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             strip.background = element_blank(),
             strip.text = element_text(size = 12))

# Figure 1 ----------------------------------------------------------------


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


