orderly2::orderly_parameters(calibrated = TRUE, repetitions = 20)
orderly2::orderly_artefact("maps", c("averted.pdf", 
                                     "proportion.pdf", 
                                     "per_child.pdf"))
# pull in the dependency from analyse_impact
# calibrated <- this:calibrated
# repetitions <- this:repetitions

library(data.table)
orderly2::orderly_resource("shape_file.RDS")
orderly2::orderly_resource("districts.RDS")
orderly2::orderly_resource("rainfall_data.RDS")

# start with the model outputs
# read in all districts
districts <- readRDS("districts.RDS")
districts <- districts[districts != "Kampala"] # exclude Kampala b/c EIR = 0

output <- data.table()
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
  counterfactual <- rbind(counterfactual, dt, fill = T)
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
  benefit <- rbind(benefit, dt, fill = T)
}

# save these so I don't have to loop over dependencies every time my computer restarts itself...
saveRDS(output, "input/output.RDS")
saveRDS(counterfactual, "input/counterfactual.RDS")
saveRDS(benefit, "input/benefit.RDS")

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
df_comb$cycles <- factor(df_comb$cycles)

# fairly certain that now the df_comb has already been summarised before reading into here
averted <- df_comb %>%
  dplyr::select(district, averted_50, year, cycles) %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(averted_annually_50 = mean(averted_50))
severe <- df_comb %>%
  dplyr::select(district, severe_50, year, cycles) %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(severe_annually_50 = mean(severe_50))
proportion <- df_comb %>%
  dplyr::select(district, proportion_50, year, cycles) %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(proportion_annually_50 = mean(proportion_50))
per_child <- df_comb %>%
  dplyr::select(district, per_child_50, year, cycles) %>%
  dplyr::group_by(district, cycles) %>%
  dplyr::reframe(per_child_annually_50 = mean(per_child_50)) %>%
  dplyr::filter(is.na(cycles) == FALSE)

averted <- full_join(shape, averted, join_by(ADM2_EN == district))
severe <- full_join(shape, severe, join_by(ADM2_EN == district))
proportion <- full_join(shape, proportion, join_by(ADM2_EN == district))
per_child <- full_join(shape, per_child, join_by(ADM2_EN == district))

# averted per child
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(averted, is.na(cycles) == FALSE), aes(fill = averted_annually_50)) +
  theme_bw() + facet_grid(. ~ cycles) +
  # theme(panel.background = element_rect(fill = "white")) + 
  guides(fill=guide_legend(title="Cases averted \nannually")) 
ggsave("averted.pdf", dpi = 300)
ggsave("averted.png", dpi = 300, width = 20, height = 8, units = "cm")

ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(severe, is.na(cycles) == FALSE), aes(fill = severe_annually_50)) +
  theme_bw() + facet_grid(. ~ cycles) +
  # theme(panel.background = element_rect(fill = "white")) + 
  guides(fill=guide_legend(title="Severe cases averted \nannually")) 
ggsave("severe.pdf", dpi = 300)
ggsave("severe.png", dpi = 300, width = 20, height = 8, units = "cm")

ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(proportion, is.na(cycles) == FALSE), aes(fill = proportion_annually_50)) + 
  theme_bw() + facet_grid(. ~ cycles)  +
  guides(fill=guide_legend(title="Proportion of cases \naverted annually")) 
ggsave("proportion.pdf", dpi = 300)
ggsave("proportion.png", dpi = 300, width = 20, height = 8, units = "cm")

ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = filter(per_child, is.na(cycles) == FALSE), 
          aes(fill = per_child_annually_50)) +
  theme_bw() + facet_grid(. ~ cycles)  +
  guides(fill=guide_legend(title="Cases averted \nper child per year")) 
ggsave("per_child.pdf", dpi = 300)
ggsave("per_child.png", dpi = 300, width = 20, height = 8, units = "cm")

## 4 cycles baseline
baseline <- counterfactual %>%
  dplyr::select(year, district, per_child_2.5:per_child_97.5) %>%
  dplyr::filter(year == 3)
baseline <- full_join(shape, baseline, join_by(ADM2_EN == district))
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = baseline, aes(fill = per_child_50)) +
  theme_bw() +
  guides(fill=guide_legend(title="Cases averted \nper child per year")) 
ggsave("4_cycle_baseline.pdf", dpi = 300)
ggsave("4_cycle_baseline.png", dpi = 300, width = 20, height = 8, units = "cm")

no_smc <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i]
  metadata <- orderly2::orderly_dependency("run_counterfactual",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20)),
                                           c(df.RDS = "df.RDS"))
  dt <- readRDS(metadata$files$here)
  no_smc <- rbind(no_smc, dt, fill = T)
}

# seasonality by district

model_seasonality_no_smc <- function(model_output_no_smc, district, months = 5) {
  data <- model_output_no_smc %>%
    dplyr::filter(district == district) %>%
    dplyr::group_by(timestep) %>%
    dplyr::reframe(cases = mean(n_inc_clinical_1_1825),
                   n_1_1825 = median(n_1_1825))
  
  years <- length(unique(data$timestep))/365
  len <- length(unique(data$timestep))
  annual_cases <- sum(data$cases/data$n_1_1825, na.rm = TRUE)/years
  days <- round(months * 30.25, digits = 0)
  
  prop <- numeric(0)
  for(i in 1:(len - (days-1))) {
    prop[i] <- (sum(data$cases[i:(i+(days - 1))]/data$n_1_1825[i:(i+(days - 1))], 
                    na.rm = TRUE))/annual_cases
  }
  
  max <- max(prop)*100
  return(max)
  
}

counterfactual_seasonality <- data.frame(district = unique(no_smc$district),
                                         five_month = numeric(length(unique(no_smc$district))))
for(i in 1:length(unique(no_smc$district))) {
  df <- data.frame(dplyr::filter(no_smc,
                                 district == counterfactual_seasonality$district[i]))
  counterfactual_seasonality$five_month[i] <- model_seasonality_no_smc(model_output_no_smc = df, 
                                                                       months = 5)
}

seasonality <- counterfactual_seasonality %>%
  dplyr::arrange(desc(five_month))

# map seasonality
counterfactual_seasonality <-  full_join(shape, counterfactual_seasonality, join_by(ADM2_EN == district))
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = counterfactual_seasonality, 
          aes(fill = five_month)) +
  theme_bw() +
  guides(fill=guide_legend(title="Model estimated seasonality \nClinical cases in 5 months")) +
  theme(legend.position = "bottom")

ggsave("seasonality.pdf", dpi = 300)
ggsave("seasonality.png", dpi = 300)

model_seasonality <- ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = counterfactual_seasonality, 
          aes(fill = five_month)) +
  theme_bw() +
  guides(fill=guide_legend(title="Model estimated seasonality \nClinical cases in 5 months")) 

rainfall <- readRDS("rainfall_data.RDS")
rainfall <- rainfall %>%
  tidyr::pivot_longer(cols = raster1:raster365, names_to = "day", values_to = "rainfall") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(day = as.numeric(unlist(strsplit(day, "raster"))[2])) %>%
  dplyr::group_by(day, districts) %>%
  dplyr::reframe(rainfall = sum(rainfall)) %>%
  dplyr::arrange(districts) %>%
  dplyr::distinct()

rainfall_seasonality_fn <- function(rainfall_df, district, months = 5) {
  data <- rainfall_df %>%
    dplyr::filter(districts == district)
  data <- rbind(data, data) %>%
    dplyr::mutate(day = row_number())
  
  years <- length(unique(data$day))/365
  len <- length(unique(data$day))
  annual_rain <- sum(data$rainfall)/years
  days <- round(months * 30.25, digits = 0)
  
  prop <- numeric(0)
  for(i in 1:(len - (days-1))) {
    prop[i] <- (sum(data$rainfall[i:(i+(days - 1))], na.rm = TRUE))/annual_rain
  }
  
  max <- max(prop)
  return(max)
  
}

rainfall_seasonality <- data.frame(district = unique(no_smc$district),
                                   five_month = numeric(length(unique(no_smc$district))))
for(i in 1:length(unique(no_smc$district))) {
  rainfall_seasonality$five_month[i] <- rainfall_seasonality_fn(rainfall_df = rainfall, 
                                                             district = rainfall_seasonality$district[i],
                                                             months = 4)*100
}
rainfall_seasonality <-  full_join(shape, rainfall_seasonality, join_by(ADM2_EN == district))
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = rainfall_seasonality, 
          aes(fill = five_month)) +
  theme_bw() +
  guides(fill=guide_legend(title="Rainfall seasonality in \n4 consecutive months")) +
  theme(legend.position = "bottom")
ggsave("rainfall_seasonality.png", dpi = 300)

prev <- data.frame(district = unique(no_smc$district),
                  prev = numeric(length(unique(no_smc$district))),
                  eir = numeric(length(unique(no_smc$district))))
for(i in 1:nrow(prev)) {
  district <- districts[i]
  metadata <- orderly2::orderly_dependency("calibrate_eir",
                                           quote(latest(parameter:district == environment:district)),
                                           c(site.RDS = "calibrated_site.RDS"))
  dt <- readRDS(metadata$files$here)
  prev$prev[i] <- dt$prevalence %>% dplyr::filter(year == 2022) %>% dplyr::pull(pfpr)
  prev$eir[i] <- as.numeric(dt$eir$eir)
}
prev <-  full_join(shape, prev, join_by(ADM2_EN == district))
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = prev, 
          aes(fill = prev*100)) +
  theme_bw() + scale_fill_continuous() +
  guides(fill=guide_legend(title="Model estimated prevalence (%)")) +
  theme(legend.position = "bottom")
ggsave("prevalence.png", dpi = 300)

# ## incidence
# incidence_df <- data.frame(district = unique(no_smc$district),
#                         incidence = numeric(length(unique(no_smc$district))))
# for(i in 1:nrow(incidence)) {
#   district <- districts[i]
#   metadata <- orderly2::orderly_dependency("analyse_impact",
#                                            quote(latest(parameter:district == environment:district)),
#                                            c(burden.RDS = "burden.RDS"))
#   dt <- readRDS(metadata$files$here)
#   incidence_df$incidence[i] <- dt$incidence_50
# }
# 
# incidence_classification <- function(incidence) {
#   if(incidence <= 13) {
#     classification <- "0 to 13"
#   } else if (incidence > 13 && incidence <= 21) {
#     classification <- "13 to 21"
#   } else if (incidence > 21 && incidence <= 34) {
#     classification <- "21 to 34"
#   } else if (incidence > 34 && incidence <= 128) {
#     classification <- "34 to 128"
#   } else if (incidence > 128) {
#     classification <- "128 to 999"
#   }
#   return(classification)
# }
# 
# incidence <- incidence_df %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(classification = incidence_classification(incidence = incidence))
# incidence$classification <- factor(incidence$classification,
#                                    levels = c("0 to 13",
#                                               "13 to 21",
#                                               "21 to 34",
#                                               "34 to 128",
#                                                "128 to 999"))
# 
# incidence <- full_join(shape, incidence, join_by(ADM2_EN == district))
# ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
#   geom_sf(data = incidence, 
#           aes(fill = classification)) +
#   theme_bw() + scale_fill_brewer() +
#   guides(fill=guide_legend(title="Model estimated incidence rate")) +
#   theme(legend.position = "bottom")
# ggsave("incidence.png", dpi = 300)
