## assess the country level impact of the intervention -- this is not the same as the sum of the districts
## outputted from analyse impact due to the credible intervals

library(data.table)
library(tidyverse)

orderly2::orderly_parameters(calibrated = TRUE, repetitions = 20)

# define artefacts
orderly2::orderly_artefact("Combined counterfactual - all reps and districts", "no_smc.RDS")
orderly2::orderly_artefact("Combined counterfactual - all reps, cycles and districts", "smc.RDS")
orderly2::orderly_artefact("Summary output - all cycles and districts + quantiles", "smc_summary.RDS")
orderly2::orderly_artefact("Burden reduction at national level split by cycles + quantiles", "national_impact.RDS")
orderly2::orderly_artefact("Burden reduction at district level split by cycles", "no_smc_district.RDS")
orderly2::orderly_artefact("SMC output at a district level - all reps and cycles", "smc_district.RDS")
orderly2::orderly_artefact("SMC impact at a district level - all districts and cycles + quantiles", "district_impact.RDS")
orderly2::orderly_artefact("Model estimated district seasonality", "seasonality.RDS")
orderly2::orderly_artefact("Proportional impact of additional cycles - 4 cycle baseline", "incremental_district.RDS")
orderly2::orderly_artefact("Population estimates and doses delivered", "doses_district")
orderly2::orderly_artefact("Doses per clinical and severe case averted - district level", "dose_per_averted")

## define resources and dependencies
orderly2::orderly_shared_resource(districts.RDS = "districts.RDS")
orderly2::orderly_resource("functions.R")
source("functions.R")

districts <- readRDS("districts.RDS")
districts <- districts[districts != "Kampala"]

# combine all the counterfactual outputs ready for data manipulation and moving to other tasks
no_smc <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i]

  files <- c("df.RDS")
  names(files) <- file.path("data", paste0("df_", district, ".RDS"))

  metadata <- orderly2::orderly_dependency("run_counterfactual",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20)),
                                           files)
  dt <- readRDS(metadata$files$here)
  no_smc <- rbind(no_smc, dt, fill = T)
  
}

# combine all the SMC outputs ready for data manipulation and moving to other tasks
smc_4 <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_smc.RDS")
  names(files) <- file.path("data", paste0("df_smc_4_", district, ".RDS"))
  
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20 &&
                                                          parameter:cycles == 4)),
                                           files)
  dt <- readRDS(metadata$files$here)
  smc_4 <- rbind(smc_4, dt, fill = T)
  
}

smc_5 <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_smc.RDS")
  names(files) <- file.path("data", paste0("df_smc_5_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                        parameter:calibrated == TRUE &&
                                                        parameter:repetitions == 20 &&
                                                        parameter:cycles == 5)),
                                           files)
  dt <- readRDS(metadata$files$here)
  smc_5 <- rbind(smc_5, dt, fill = T)
  
}

smc_6 <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_smc.RDS")
  names(files) <- file.path("data", paste0("df_smc_6_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20 &&
                                                          parameter:cycles == 6)),
                                           files)
  dt <- readRDS(metadata$files$here)
  smc_6 <- rbind(smc_6, dt, fill = T)
  
}

smc_7 <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_smc.RDS")
  names(files) <- file.path("data", paste0("df_smc_7_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20 &&
                                                          parameter:cycles == 7)),
                                           files)
  dt <- readRDS(metadata$files$here)
  smc_7 <- rbind(smc_7, dt, fill = T)
  
}

# combine into one df
smc_4$cycles <- "4 cycles"
smc_5$cycles <- "5 cycles"
smc_6$cycles <- "6 cycles"
smc_7$cycles <- "7 cycles"
smc <- rbind(smc_4, smc_5, smc_6, smc_7)

# summary data of SMC implementation
output <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_comb.RDS")
  names(files) <- file.path("data", paste0("df_comb_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("analyse_impact",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20)),
                                           c(df.RDS = "df_comb.RDS"))
  dt <- readRDS(metadata$files$here)
  output <- rbind(output, dt, fill = T)
}

no_smc_national <- no_smc %>%
  dplyr::filter(timestep >= 730) %>%
  dplyr::select(district, repetition, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(repetition) %>%
  dplyr::reframe(cases = sum(n_inc_clinical_1_1825),
                 severe = sum(n_inc_severe_1_1825))
smc_national <- smc %>%
  dplyr::filter(timestep >= 730) %>%
  dplyr::select(district, repetition, cycles, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(repetition, cycles) %>%
  dplyr::reframe(cases_smc = sum(n_inc_clinical_1_1825),
                 severe_smc = sum(n_inc_severe_1_1825))

national_impact <- full_join(no_smc_national, smc_national) %>%
  dplyr::mutate(averted = cases - cases_smc,
                severe_averted = severe - severe_smc, 
                prop = averted/cases,
                prop_severe = severe_averted/severe) %>%
  dplyr::group_by(cycles) %>%
  dplyr::reframe(averted_2.5 = quantile(averted, 0.025),
                 averted_50 = median(averted),
                 averted_97.5 = quantile(averted, 0.975),
                 severe_2.5 = quantile(severe_averted, 0.025),
                 severe_50 = median(severe_averted),
                 severe_97.5 = quantile(severe_averted, 0.975),
                 prop_averted_2.5 = quantile(prop, 0.025),
                 prop_averted_50 = median(prop),
                 prop_averted_97.5 = quantile(prop, 0.975),
                 prop_severe_2.5 = quantile(prop_severe, 0.025),
                 prop_severe_50 = median(prop_severe),
                 prop_severe_97.5 = quantile(prop_severe, 0.975))

no_smc_district <- no_smc %>%
  dplyr::filter(timestep >= 730) %>%
  dplyr::select(district, repetition, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(district, repetition) %>%
  dplyr::reframe(cases = sum(n_inc_clinical_1_1825),
                 severe = sum(n_inc_severe_1_1825))
smc_district <- smc %>%
  dplyr::filter(timestep >= 730) %>%
  dplyr::select(district, repetition, cycles, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(district, repetition, cycles) %>%
  dplyr::reframe(cases_smc = sum(n_inc_clinical_1_1825),
                 severe_smc = sum(n_inc_severe_1_1825))

district_impact <- full_join(no_smc_district, smc_district) %>%
  dplyr::mutate(averted = cases - cases_smc,
                severe_averted = severe - severe_smc, 
                prop = averted/cases,
                prop_severe = severe_averted/severe) %>%
  dplyr::group_by(cycles, district) %>%
  dplyr::reframe(averted_2.5 = quantile(averted, 0.025),
                 averted_50 = median(averted),
                 averted_97.5 = quantile(averted, 0.975),
                 severe_2.5 = quantile(severe_averted, 0.025),
                 severe_50 = median(severe_averted),
                 severe_97.5 = quantile(severe_averted, 0.975),
                 prop_averted_2.5 = quantile(prop, 0.025),
                 prop_averted_50 = median(prop),
                 prop_averted_97.5 = quantile(prop, 0.975),
                 prop_severe_2.5 = quantile(prop_severe, 0.025),
                 prop_severe_50 = median(prop_severe),
                 prop_severe_97.5 = quantile(prop_severe, 0.975))

incremental_district <- district_impact %>%
  dplyr::select(district, averted_50, cycles) %>%
  tidyr::pivot_wider(names_from = cycles, values_from = averted_50) %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(averted_4 = `4 cycles`,
                 averted_5 = `5 cycles`,
                 averted_6 = `6 cycles`,
                 averted_7 = `7 cycles`) %>%
  dplyr::mutate(incremental_5 = (averted_5 - averted_4)/averted_4,
                incremental_6 = (averted_6 - averted_4)/averted_4,
                incremental_7 = (averted_7 - averted_4)/averted_4) %>%
  dplyr::select(district, incremental_5:incremental_7) %>%
  pivot_longer(incremental_5:incremental_7, 
               names_to = "cycles",
               values_to = "proportional_impact") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(cycles = as.numeric(gsub('incremental_','', cycles))) %>%
  dplyr::mutate(cycles = paste(cycles, "cycles"))


# seasonality in each district by repetition
seasonality <- data.frame(district = character(0), repetitions = numeric(0), seasonality = numeric(0))
for(i in 1:length(districts)) {
  for(j in 1:20) {
    out <- no_smc %>%
      dplyr::filter(district == districts[i]) %>%
      dplyr::filter(repetition == j) %>%
      dplyr::filter(timestep >= 730)
    metric <- model_seasonality_func(out)
    vec <- c(districts[i], j, metric)
    seasonality <- rbind(seasonality, vec)
  }
}

names(seasonality) <- c("district", "repetitions", "seasonality")
seasonality$seasonality <- as.numeric(seasonality$seasonality)
seasonality$district <- factor(seasonality$district)
seasonality$repetitions <- factor(seasonality$repetitions)

seasonality_district <- seasonality %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(seasonality_2.5 = quantile(seasonality, 0.025),
                 seasonality_50 = median(seasonality),
                 seasonality_97.5 = quantile(seasonality, 0.975)) 

population <- data.frame(district = unique(no_smc$district),
                         population = numeric(length(unique(no_smc$district))))
for(i in 1:nrow(population)) {
  district <- population$district[i]
  metadata <- orderly2::orderly_dependency("calibrate_eir",
                                           quote(latest(parameter:district == environment:district)),
                                           c(site.RDS = "calibrated_site.RDS"))
  dt <- readRDS(metadata$files$here)
  population$population[i] <- dt$population %>% dplyr::filter(year == 2022) %>% pull(par_pf)
}

u5 <- 0.1677317 # from UN WPP estimates for Uganda

doses_district <- population %>%
  dplyr::mutate(under_5 = u5,
                coverage = "90%",
                pop_under_5 = population * u5,
                doses_4 = population * u5 * 0.9 * 4,
                doses_5 = population * u5 * 0.9 * 5,
                doses_6 = population * u5 * 0.9 * 6,
                doses_7 = population * u5 * 0.9 * 7) %>%
  tidyr::pivot_longer(doses_4:doses_7, names_to = "cycles", values_to = "doses") %>%
  dplyr::mutate(cycles = paste(gsub("doses_","",cycles), "cycles"))

dose_per_averted <- full_join(doses_district, district_impact) %>%
  dplyr::group_by(district, cycles) %>% 
  dplyr::reframe(dose_per_clin = doses/averted_50,
                 dose_per_sev = doses/severe_50)

# outputs 
saveRDS(no_smc, "no_smc.RDS")
saveRDS(smc, "smc.RDS")
saveRDS(output, "smc_summary.RDS")
saveRDS(national_impact, "national_impact.RDS")
saveRDS(no_smc_district, "no_smc_district.RDS")
saveRDS(smc_district, "smc_district.RDS")
saveRDS(district_impact, "district_impact.RDS")
saveRDS(seasonality_district, "seasonality.RDS")
saveRDS(incremental_district, "incremental_district.RDS")
saveRDS(doses_district, "doses_district")
saveRDS(dose_per_averted, "dose_per_averted")
