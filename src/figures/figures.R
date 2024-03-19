# generate all figures for thesis/manuscripts
orderly2::orderly_strict_mode()
orderly2::orderly_parameters(calibrated = TRUE, repetitions = 20)
# pull in the dependency from analyse_impact
# calibrated <- this:calibrated
# repetitions <- this:repetitions

library(data.table)

# start with the model outputs
# read in all districts
districts <- readRDS("districts.RDS")
districts <- districts[districts != "Kampala"] # exclude Kampala b/c EIR = 0

output<- data.table()
for(i in 1:length(districts)) {
  district <- districts[i]
  metadata <- orderly2::orderly_dependency("analyse_impact",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20)),
                                           c(df.RDS = "df_comb.RDS"))
  dt <- readRDS(metadata$files$here)
  output <- rbind(output, dt, fill = T)
}
counterfactual <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i]
  metadata <- orderly2::orderly_dependency("analyse_impact",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20)),
                                           c(baseline.RDS = "baseline.RDS"))
  dt <- readRDS(metadata$files$here)
  output <- rbind(counterfactual, dt, fill = T)
}
benefit <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i]
  metadata <- orderly2::orderly_dependency("analyse_impact",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20)),
                                           c(incremental.RDS = "incremental.RDS"))
  dt <- readRDS(metadata$files$here)
  output <- rbind(benefit, dt, fill = T)
}

orderly2::orderly_resource("shape_file.RDS")

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
df_comb <- output

karamoja_dist <- c("Abim", "Amudat", "Kaabong", 
                   "Karenga", "Kotido", "Moroto", 
                   "Nabilatuk", "Nakapiripirit", "Napak")
karamoja_shp <- shape %>%
  dplyr::filter(ADM2_EN %in% karamoja_dist)

karamoja_int <- tibble(district = karamoja_dist,
                       intervention_2021 = c("No SMC",
                                             "No SMC",
                                             "No SMC",
                                             "No SMC",
                                             "Intervention district",
                                             "Intervention district",
                                             "Control district",
                                             "No SMC",
                                             "No SMC"),
                       intervention_2022 = c("No SMC",
                                             "cRCT trial district",
                                             "No SMC",
                                             "No SMC",
                                             "Trial district",
                                             "Trial district",
                                             "Trial district",
                                             "Trial district",
                                             "No SMC"))
int_2021 <- karamoja_int[,1:2]
int_2022 <- karamoja_int[,c(1,3)]

karamoja_2021 <- dplyr::left_join(karamoja_shp, int_2021, by=join_by(ADM2_EN==district))
karamoja_2021$intervention <- factor(karamoja_2021$intervention_2021,
                                     levels = c("Intervention district",
                                                "Control district",
                                                "No SMC"))
karamoja_2021$year <- "2021"
karamoja_2021 <- karamoja_2021 %>% dplyr::select(-intervention_2021)

karamoja_2022 <- dplyr::left_join(karamoja_shp, int_2022, by=join_by(ADM2_EN==district))
karamoja_2022$intervention <- factor(karamoja_2022$intervention_2022,
                                     levels = c("cRCT trial district",
                                                "Trial district",
                                                "No SMC"))
karamoja_2022$year <- "2022"
karamoja_2022 <- karamoja_2022 %>% dplyr::select(-intervention_2022)

karamoja <- rbind(karamoja_2021, karamoja_2022)
karamoja$year <- factor(karamoja$year)
karamoja$intervention

cols <- c("#a6cee3", "#1f78b4","#9ba2ff", "#b2df8a", "#3E4E8E")
names(cols) <- c("Intervention district",
                 "Control district",
                 "Trial district",
                 "No SMC",
                 "cRCT trial district")
col_scale <- scale_colour_manual(name = "intervention",values = cols)
fill_scale <- scale_fill_manual(name = "intervention",values = cols)

ggplot() + geom_sf(data = karamoja, aes(fill = intervention), lwd = 0.6) + theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 10)) + fill_scale + 
  guides(fill=guide_legend(title="Intervention")) + 
  # theme(legend.position = "bottom",
  #       legend.direction = "vertical") + 
  facet_grid(. ~ year) 


ggsave("output/trial_map.png", dpi = 300, width = 20, height = 20, units = "cm")

# # averted per child
# ggplot() + geom_sf(data = adm1, fill = "grey85", lwd = 0.4) +
#   geom_sf(data = test, aes(fill = per_child_50)) +
#   theme(panel.background = element_rect(fill = "white")) #+
#   # geom_sf(data = karamoja, col = "red", alpha = 0, lwd = 0.4)
#   
# # update this after full model run
# #proportion of cases averted annually
# ggplot() + geom_sf(data = adm1, fill = "grey85", lwd = 0.4) +
#   geom_sf(data = test, aes(fill = proportion_50)) +
#   theme(panel.background = element_rect(fill = "white")) +
#   geom_sf(data = karamoja, col = "red", alpha = 0, lwd = 0.4)
# 
