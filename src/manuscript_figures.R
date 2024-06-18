## fix colour schemes
mybreaks <- seq(0, 10000,2000)
per_child$`Clinical cases averted per 10000 children` = per_child$per_child_annually_50*10000

get_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend) }

plot_A <- ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(per_child, is.na(cycles) == FALSE), 
          aes(fill = `Clinical cases averted per 10000 children`)) +
  theme_bw() + facet_grid(. ~ cycles) + 
  # guides(fill=guide_legend(title="Clinical cases averted \nper 10,000 children annually")) +
  theme(legend.position = "bottom") + geom_tile() +
  # scale_fill_gradientn(
  #   colours = hcl.colors(length(mybreaks)-1, "Zissou1", rev = FALSE), 
  #   breaks = mybreaks
  # ) + 
  theme(legend.key.size = unit(1.5, 'cm')) +
  scale_fill_viridis_c(option = "magma")
legend_A <- get_legend(plot_A)
panel_A <- plot_A + theme(legend.position = "none")
top_panel <- ggarrange(panel_A, legend_A, labels = c("A"), nrow = 2, heights = c(1, 0.2))

# ggsave("panel_1.png", dpi = 300,width = 40, height = 20, units = "cm")

# ggsave("panel_A.png", dpi = 300, height = 10, width = 22, units = "cm")
averted_4 <- averted %>%
  dplyr::filter(cycles == "4 cycles") %>%
  dplyr::mutate(`Cases averted annually \nwith 4 cycles` = averted_annually_50)

plot_B <- ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(averted_4, cycles == "4 cycles"), 
          aes(fill = `Cases averted annually \nwith 4 cycles`)) +
  theme_bw() + scale_fill_viridis_c() +
  theme(legend.key.size = unit(1, 'cm')) + 
  theme(legend.position = "bottom") + facet_grid(. ~ cycles)
legend_B <- get_legend(plot_B)
panel_B <- plot_B + theme(legend.position = "none")
# ggsave("panel_B.png", dpi = 300, height = 10, width = 8, units = "cm")

incremental <- incremental %>%
  dplyr::mutate(`% additional cases \n averted vs 4 cycles` = proportional_impact * 100)

mybreaks <- seq(-10, 80, 10)
plot_C <- ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(incremental, is.na(cycles) == FALSE), 
          aes(fill = `% additional cases \n averted vs 4 cycles`)) +
  theme_bw() + facet_grid(. ~ cycles) +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks)-1, "Zissou1", rev = FALSE), 
    breaks = mybreaks
  ) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(1, 'cm'))
legend_C <- get_legend(plot_C)
panel_C <- plot_C + theme(legend.position = "none")

bottom_panel <- ggarrange(panel_B, panel_C, legend_B, legend_C,
                          labels = c("B", "C", "", ""),
                          nrow = 2, ncol = 2, heights = c(1, 0.2), widths = c(1, 2.85))
ggarrange(top_panel, bottom_panel, nrow = 2)
ggsave("multipanel.png", dpi = 300, width = 375, height = 250, units = "mm")

seasonal_dist <- seasonality %>%
  dplyr::group_by(ADM2_EN) %>%
  dplyr::reframe(metric = median(seasonality_50)) %>%
  dplyr::filter(metric >= 60) %>%
  dplyr::pull(ADM2_EN)

doses_per_averted$implementation <- "national"
doses_per_averted_seasonal <- doses_per_averted %>%
  dplyr::filter(district %in% seasonal_dist) %>%
  dplyr::mutate(implementation = "seasonal")
doses_plot <- rbind(doses_per_averted, doses_per_averted_seasonal) %>%
  dplyr::group_by(district, cycles, implementation) %>%
  dplyr::reframe(clinical = dose_per_clin,
                 severe = dose_per_sev) %>%
  tidyr::pivot_longer(clinical:severe, names_to = "disease class", values_to = "Cycles administered \nper case averted")
doses_plot <- full_join(shape, doses_plot, join_by(ADM2_EN == district)) %>%
  dplyr::filter(cycles == "5 cycles")

mybreaks <- seq(0, 300,50)
dose_A <- ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(doses_plot, `disease class` == "clinical"), 
          aes(fill = `Cycles administered \nper case averted`)) +
  theme_bw() + facet_grid(. ~ implementation) +
  scale_fill_gradientn(
    colours = hcl.colors(length(mybreaks)-1, "Zissou1", rev = FALSE), 
    breaks = mybreaks
  ) +
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(1, 'cm'))

dose_B <- ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(doses_plot, `disease class` == "clinical"), 
          aes(fill = `Cycles administered \nper case averted`)) +
  theme_bw() + facet_grid(. ~ implementation) + scale_fill_viridis_c(direction = -1) + 
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(1, 'cm')) 

ggarrange(dose_A, dose_B, labels = c("A", "B"), nrow = 2)
ggsave("doses_seasonal.png", dpi = 300, width = 180, height = 250, units = "mm")

tot_pop <- population %>% dplyr::select(ADM2_EN, subpopulation, population_size) %>% 
  dplyr::group_by(subpopulation) %>%
  dplyr::reframe(total = sum(population_size, na.rm = T))
seasonal_pop <- population %>% dplyr::select(ADM2_EN, subpopulation, population_size) %>% 
  dplyr::filter(ADM2_EN %in% seasonal_dist) %>%
  dplyr::group_by(subpopulation) %>%
  dplyr::reframe(total = sum(population_size, na.rm = T))

district_impact %>% dplyr::filter(cycles == "5 cycles") %>%
  dplyr::reframe(averted_med = median(averted_50),
                 averted_low = quantile(averted_50, 0.025),
                 averted_upp = quantile(averted_50, 0.975),
                 severe_med = median(severe_50),
                 severe_low = quantile(severe_50, 0.025),
                 severe_upp = quantile(severe_50, 0.975))

no_smc %>% 
  dplyr::filter(district %in% seasonal_dist) %>%
  dplyr::group_by(district, timestep, repetition) %>%
  dplyr::reframe(incidence = n_inc_clinical_1_1825/n_1_1825) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(district, repetition) %>%
  dplyr::filter(timestep < 366) %>%
  dplyr::reframe(incidence = sum(incidence)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(incidence_2.5 = quantile(incidence, 0.025),
                 incidence_50 = median(incidence),
                 incidence_97.5 = quantile(incidence, 0.975)) %>%
  dplyr::ungroup() %>%
  dplyr::reframe(median = median(incidence_50),
                 lower = quantile(incidence_50, 0.025),
                 upper = quantile(incidence_50, 0.975))

averted_seasonal <- averted %>% 
  dplyr::filter(district %in% seasonal_dist) %>%
  dplyr::filter(cycles == "5 cycles") %>%
  dplyr::reframe(averted = sum(averted_annually_50)) %>%
  dplyr::pull(averted)
severe_seasonal <- severe %>% 
  dplyr::filter(district %in% seasonal_dist) %>%
  dplyr::filter(cycles == "5 cycles") %>%
  dplyr::reframe(averted = sum(severe_annually_50))  %>%
  dplyr::pull(averted)

averted %>% 
  dplyr::filter(cycles == "5 cycles") %>%
  dplyr::reframe(averted = sum(averted_annually_50)) %>%
  dplyr::pull(averted)
severe %>% 
  dplyr::filter(cycles == "5 cycles") %>%
  dplyr::reframe(averted = sum(severe_annually_50)) %>%
  dplyr::pull(averted)

burden_national <- no_smc_district %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(cases = median(cases),
                 severe = median(severe)) %>%
  dplyr::ungroup() %>%
  dplyr::reframe(tot_cases = sum(cases),
                 tot_sev = sum(severe))

doses_per_averted %>%
  dplyr::filter(cycles == "5 cycles") %>%
  dplyr::reframe(clin_50 = median(dose_per_clin),
                 clin_2.5 = quantile(dose_per_clin, 0.025),
                 clin_97.5 = quantile(dose_per_clin, 0.975),
                 sev_50 = median(dose_per_sev),
                 sev_2.5 = quantile(dose_per_sev, 0.025),
                 sev_97.5 = quantile(dose_per_sev, 0.975))

dist_30 <- per_child %>%
  dplyr::filter(cycles == "5 cycles") %>%
  dplyr::arrange(desc(per_child_annually_50)) %>%
  dplyr::slice_head(n = 30) %>%
  dplyr::pull(district)

averted %>% 
  dplyr::filter(ADM2_EN %in% dist_30) %>%
  dplyr::filter(cycles == "5 cycles") %>%
  dplyr::reframe(averted = sum(averted_annually_50)) %>%
  dplyr::pull(averted)
  
top_30 <- per_child %>%
  dplyr::filter(district %in% dist_30) %>%
  dplyr::filter(cycles == "5 cycles") %>%
  dplyr::left_join(population) %>%
  dplyr::filter(subpopulation == "under_5") %>%
  dplyr::select(district, population_size, per_child_annually_50) %>%
  dplyr::arrange(desc(per_child_annually_50)) %>%
  dplyr::left_join(population_prev) %>%
  dplyr::left_join(seasonality) %>%
  dplyr::select(-c(eir, seasonality_2.5, seasonality_97.5))
write.csv(top_30, "top_30.csv", row.names = TRUE)







