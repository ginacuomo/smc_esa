# functions used in Mozambique impact task
quantile_95 <- function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}

# plot trajectories
library(ggplot2)
library(tidyverse)

plot_trajectory <- function(q95_df, dist) {
  
  combined_q95 <- q95_df |>
    dplyr::filter(district == dist)
  
  # "Monapo"     "Mongincual" "Mossuril"   "Muecate"    "Nampula"
  
  # combined_q95 has the median + 95% CrI at all time points
  # for both smc and no smc combined into one long df
  ggplot(combined_q95) + 
    geom_line(aes(x = time, y = `50%`*100, col = intervention), lwd = 0.6) + 
    geom_ribbon(aes(x = time, ymin = `2.5%`*100, ymax = `97.5%`*100, 
                    fill = intervention), alpha = 0.2) + 
    theme_bw() + guides(fill=guide_legend(title="Intervention")) +
    guides(col=guide_legend(title="Intervention")) +
    labs(x = "Time (days)", 
         y = "Clinical infections") + 
    # times = vector of SMC delivery dates
    # geom_vline(xintercept = times, lty = 2, lwd = 0.8) + 
    xlim(c(0, 1000)) + theme(axis.text = element_text(size = 12),
                            axis.title = element_text(size = 12),
                            legend.text = element_text(size = 12),
                            legend.title = element_text(size = 12),
                            legend.position = "bottom") +
    labs(subtitle = dist) + expand_limits(y = 0)
}
