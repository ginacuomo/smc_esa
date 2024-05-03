orderly2::orderly_parameters(calibrated = TRUE, repetitions = 20)
orderly2::orderly_artefact("maps", c("averted.pdf", 
                                     "proportion.pdf", 
                                     "per_child.pdf"))
# pull in the dependency from country_impact
# calibrated <- this:calibrated
# repetitions <- this:repetitions

library(data.table)
orderly2::orderly_shared_resource(shape_file.RDS = "shape_file.RDS")
orderly2::orderly_shared_resource(districts.RDS = "districts.RDS")
orderly2::orderly_resource("rainfall_data.RDS")

orderly2::orderly_dependency("country_impact", quote(latest(parameter:calibrated == TRUE &&
                                                              parameter:repetitions == 20)), 
                             c(no_smc.RDS = "no_smc.RDS",
                               smc.RDS = "smc.RDS",
                               smc_summary.RDS = "smc_summary.RDS",
                               national_impact.RDS = "national_impact.RDS",
                               no_smc_district.RDS = "no_smc_district.RDS",
                               smc_district.RDS = "smc_district.RDS",
                               district_impact.RDS = "district_impact.RDS",
                               seasonality.RDS = "seasonality.RDS",
                               incremental_district.RDS = "incremental_district.RDS",
                               doses_district.RDS = "doses_district.RDS",
                               dose_per_averted.RDS = "dose_per_averted.RDS"))

orderly2::orderly_artefact("Map of X", "map.png")


# load packages
library(tidyverse)
library(lubridate)
library(rgdal)
library(rgeos)
library(maptools)
library(sf)
library(terra)
library(ggpubr)

shape <- readRDS("shape_file.RDS")
no_smc <- readRDS("no_smc.RDS")
smc <- readRDS("smc.RDS")
df_comb <- readRDS("smc_summary.RDS") # smc_summary is equivalent to df_comb but already combined
national_impact <- readRDS("national_impact.RDS")
no_smc_district <- readRDS("no_smc_district.RDS")
smc_district <- readRDS("smc_district.RDS")
district_impact <- readRDS("district_impact.RDS")
seasonality <- readRDS("seasonality.RDS")
incremental_district <- readRDS("incremental_district.RDS")
doses_district <- readRDS("doses_district.RDS")
doses_per_averted <- readRDS("dose_per_averted.RDS")

# create plotting objects - generic plots
## burden w/o SMC
burden <- no_smc_district %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(cases_50 = median(cases),
                 cases_2.5 = quantile(cases, probs = 0.025),
                 cases_97.5 = quantile(cases, probs = 0.975),
                 severe_50 = median(severe),
                 severe_2.5 = quantile(severe, probs = 0.025),
                 severe_97.5 = quantile(severe, probs = 0.975))

averted <- df_comb %>%
  dplyr::select(district, averted_50, cycles) %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(averted_annually_50 = mean(averted_50)) %>%
  dplyr::filter(is.na(cycles) == FALSE)

severe <- df_comb %>%
  dplyr::select(district, severe_50, cycles) %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(severe_annually_50 = mean(severe_50)) %>%
  dplyr::filter(is.na(cycles) == FALSE) 

proportion <- df_comb %>%
  dplyr::select(district, proportion_50, cycles) %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(proportion_annually_50 = mean(proportion_50)) %>%
  dplyr::filter(is.na(cycles) == FALSE) 

per_child <- df_comb %>%
  dplyr::select(district, per_child_50, cycles) %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(per_child_annually_50 = mean(per_child_50)) %>%
  dplyr::filter(is.na(cycles) == FALSE) 

population <- doses_district %>%
  dplyr::select(district, population, pop_under_5) %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(population = population,
                 under_5 = pop_under_5) %>%
  tidyr::pivot_longer(population:under_5, names_to = "subpopulation", values_to = "population_size") %>%
  dplyr::distinct()

doses <- doses_district %>%
  dplyr::select(district, cycles, doses) 

doses_averted <- doses_per_averted %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(clinical = dose_per_clin,
                 severe = dose_per_sev) %>%
  tidyr::pivot_longer(clinical:severe, names_to = "case_definition", values_to = "doses_per_averted")

prev <- population_prev %>%
  dplyr::select(district, prev)

# combine with the shape file
burden <- full_join(shape, burden, join_by(ADM2_EN == district))
averted <- full_join(shape, averted, join_by(ADM2_EN == district))
severe <- full_join(shape, severe, join_by(ADM2_EN == district))
proportion <- full_join(shape, proportion, join_by(ADM2_EN == district))
per_child <- full_join(shape, per_child, join_by(ADM2_EN == district))
seasonality <- full_join(shape, seasonality, join_by(ADM2_EN == district))
incremental <- full_join(shape, incremental_district, join_by(ADM2_EN == district))
population <- full_join(shape, population, join_by(ADM2_EN == district))
doses <- full_join(shape, doses, join_by(ADM2_EN == district))
doses_averted <- full_join(shape, doses_averted, join_by(ADM2_EN == district))
prev <- full_join(shape, prev, join_by(ADM2_EN == district))

## making plots
## without SMC
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = burden, aes(fill = cases_50)) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white")) + 
  guides(fill=guide_legend(title="Clinical cases annually")) 
ggsave("no_smc_clinical.png", dpi = 300, width = 20, height = 8, units = "cm")

ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = burden, aes(fill = severe_50)) +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white")) + 
  guides(fill=guide_legend(title="severe cases annually")) 
ggsave("no_smc_severe.png", dpi = 300, width = 20, height = 8, units = "cm")

### SMC impact
# averted
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(averted, is.na(cycles) == FALSE), aes(fill = averted_annually_50)) +
  theme_bw() + facet_grid(. ~ cycles) +
  theme(panel.background = element_rect(fill = "white")) + 
  guides(fill=guide_legend(title="Cases averted \nannually")) 
ggsave("averted.png", dpi = 300, width = 20, height = 8, units = "cm")

# severe averted
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(severe, is.na(cycles) == FALSE), aes(fill = severe_annually_50)) +
  theme_bw() + facet_grid(. ~ cycles) +
  # theme(panel.background = element_rect(fill = "white")) + 
  guides(fill=guide_legend(title="Severe cases averted \nannually")) 
ggsave("severe.png", dpi = 300, width = 20, height = 8, units = "cm")

# proportion averted
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(proportion, is.na(cycles) == FALSE), aes(fill = proportion_annually_50)) + 
  theme_bw() + facet_grid(. ~ cycles)  +
  guides(fill=guide_legend(title="Proportion of cases \naverted annually")) 
ggsave("proportion.png", dpi = 300, width = 20, height = 8, units = "cm")

# averted per child
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(per_child, is.na(cycles) == FALSE), 
          aes(fill = per_child_annually_50)) +
  theme_bw() + facet_grid(. ~ cycles)   +
  guides(fill=guide_legend(title="Cases averted \nper child per year")) 
ggsave("per_child.png", dpi = 300, width = 20, height = 8, units = "cm")

# incremental benefit vs 4 cycles
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(incremental, is.na(cycles) == FALSE), 
          aes(fill = proportional_impact)) +
  theme_bw() + facet_grid(. ~ cycles)  + 
  scale_fill_viridis_c(option = "magma") +
  guides(fill=guide_legend(title="% additional cases averted")) 
ggsave("incremental.png", dpi = 300, width = 20, height = 8, units = "cm")

# doses per case averted
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(doses_averted, case_definition == "clinical"), 
          aes(fill = doses_per_averted)) +
  theme_bw() + 
  guides(fill=guide_legend(title="cycles administered per \ncases averted")) 
ggsave("doses_per_clinical.png", dpi = 300, width = 20, height = 8, units = "cm")

ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(doses_averted, case_definition == "severe"), 
          aes(fill = doses_per_averted)) +
  theme_bw() + 
  guides(fill=guide_legend(title="cycles administered per \ncases averted")) 
ggsave("doses_per_severe.png", dpi = 300, width = 20, height = 8, units = "cm")

## understanding setting
# seasonality of transmission 
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = seasonality, 
          aes(fill = seasonality_50)) +
  theme_bw() + 
  guides(fill=guide_legend(title="% clinical cases in \nfive consecutive months")) 
ggsave("seasonality.png", dpi = 300, width = 20, height = 8, units = "cm")

# prevalence
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = prev, 
          aes(fill = prev*100)) +
  theme_bw() + 
  guides(fill=guide_legend(title="District-level prevalence (%)")) 
ggsave("prev.png", dpi = 300, width = 20, height = 8, units = "cm")

# population
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(population, subpopulation == "under_5"), 
          aes(fill = population_size)) +
  theme_bw() + 
  guides(fill=guide_legend(title="SMC eligible population estimated")) 
ggsave("population.png", dpi = 300, width = 20, height = 8, units = "cm")

# need to add back in rainfall info but will do this more elegantly later 
# rainfall <- readRDS("rainfall_data.RDS")
# rainfall <- rainfall %>%
#   tidyr::pivot_longer(cols = raster1:raster365, names_to = "day", values_to = "rainfall") %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(day = as.numeric(unlist(strsplit(day, "raster"))[2])) %>%
#   dplyr::group_by(day, districts) %>%
#   dplyr::reframe(rainfall = sum(rainfall)) %>%
#   dplyr::arrange(districts) %>%
#   dplyr::distinct()
# 
# rainfall_seasonality_fn <- function(rainfall_df, district, months = 3) {
#   data <- rainfall_df %>%
#     dplyr::filter(districts == district)
#   data <- rbind(data, data) %>%
#     dplyr::mutate(day = row_number())
#   
#   years <- length(unique(data$day))/365
#   len <- length(unique(data$day))
#   annual_rain <- sum(data$rainfall)/years
#   days <- round(months * 30.25, digits = 0)
#   
#   prop <- numeric(0)
#   for(i in 1:(len - (days-1))) {
#     prop[i] <- (sum(data$rainfall[i:(i+(days - 1))], na.rm = TRUE))/annual_rain
#   }
#   
#   max <- max(prop)
#   return(max)
#   
# }
# 
# rainfall_seasonality <- data.frame(district = unique(no_smc$district),
#                                    three_month = numeric(length(unique(no_smc$district))))
# for(i in 1:length(unique(no_smc$district))) {
#   rainfall_seasonality$three_month[i] <- rainfall_seasonality_fn(rainfall_df = rainfall, 
#                                                              district = rainfall_seasonality$district[i],
#                                                              months = 3)*100
# }
# rainfall_seasonality <-  full_join(shape, rainfall_seasonality, join_by(ADM2_EN == district))
# ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
#   geom_sf(data = rainfall_seasonality, 
#           aes(fill = three_month)) +
#   theme_bw() +
#   guides(fill=guide_legend(title="Rainfall seasonality in \n3 consecutive months")) +
#   theme(legend.position = "bottom")
# ggsave("rainfall_seasonality.png", dpi = 300)

