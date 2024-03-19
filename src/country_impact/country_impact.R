## assess the country level impact of the intervention -- this is not the same as the sum of the districts
## outputted from analyse impact due to the credible intervals

library(data.table)
library(tidyverse)

orderly2::orderly_resource("districts.RDS")
districts <- readRDS("districts.RDS")
districts <- districts[districts != "Kampala"]

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

smc <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                        parameter:calibrated == TRUE &&
                                                        parameter:repetitions == 20 &&
                                                        parameter:cycles == 5)),
                                           c(df_smc.RDS = "df_smc.RDS"))
  dt <- readRDS(metadata$files$here)
  smc <- rbind(smc, dt, fill = T)
  
}

smc_6 <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20 &&
                                                          parameter:cycles == 6)),
                                           c(df_smc.RDS = "df_smc.RDS"))
  dt <- readRDS(metadata$files$here)
  smc_6 <- rbind(smc_6, dt, fill = T)
  
}

smc_7 <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == 20 &&
                                                          parameter:cycles == 7)),
                                           c(df_smc.RDS = "df_smc.RDS"))
  dt <- readRDS(metadata$files$here)
  smc_7 <- rbind(smc_7, dt, fill = T)
  
}

no_smc_summary <- no_smc %>%
  dplyr::select(district, repetition, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(repetition) %>%
  dplyr::reframe(cases = sum(n_inc_clinical_1_1825),
                 severe = sum(n_inc_severe_1_1825))
smc_summary <- smc %>%
  dplyr::select(district, repetition, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(repetition) %>%
  dplyr::reframe(cases_smc = sum(n_inc_clinical_1_1825),
                 severe_smc = sum(n_inc_severe_1_1825))

compare <- full_join(no_smc_summary, smc_summary) %>%
  dplyr::mutate(averted = cases - cases_smc,
                severe_averted = severe - severe_smc) %>%
  dplyr::reframe(averted_2.5 = quantile(averted, 0.025),
                 averted_50 = median(averted),
                 averted_97.5 = quantile(averted, 0.975),
                 severe_2.5 = quantile(severe_averted, 0.025),
                 severe_50 = median(severe_averted),
                 severe_97.5 = quantile(severe_averted, 0.975))

## proportion of cases averted
join_no <- no_smc %>%
  dplyr::select(district, repetition, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(repetition, district) %>%
  dplyr::reframe(cases = sum(n_inc_clinical_1_1825),
                 severe = sum(n_inc_severe_1_1825))
join_smc <- smc %>%
  dplyr::select(district, repetition, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(repetition, district) %>%
  dplyr::reframe(cases_5 = sum(n_inc_clinical_1_1825),
                 severe_5 = sum(n_inc_severe_1_1825))
join_smc_6 <- smc_6 %>%
  dplyr::select(district, repetition, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(repetition, district) %>%
  dplyr::reframe(cases_6 = sum(n_inc_clinical_1_1825),
                 severe_6 = sum(n_inc_severe_1_1825))
join_smc_7 <- smc_7 %>%
  dplyr::select(district, repetition, n_inc_clinical_1_1825, n_inc_severe_1_1825) %>%
  dplyr::group_by(repetition, district) %>%
  dplyr::reframe(cases_7 = sum(n_inc_clinical_1_1825),
                 severe_7 = sum(n_inc_severe_1_1825))
join <- full_join(join_no, join_smc) %>%
  full_join(join_smc_6) %>%
  full_join(join_smc_7) %>%
  dplyr::mutate(averted_5 = cases - cases_5,
                severe_5 = severe - severe_5,
                averted_6 = cases - cases_6,
                severe_6 = severe - severe_6,
                averted_7 = cases - cases_7,
                severe_7 = severe - severe_7,
                prop_averted_5 = (cases - cases_5)/cases,
                prop_averted_6 = (cases - cases_6)/cases,
                prop_averted_7 = (cases - cases_7)/cases) %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(prop_5_2.5 = quantile(prop_averted_5, 0.025),
                 prop_5_50 = quantile(prop_averted_5, 0.5),
                 prop_5_97.5 = quantile(prop_averted_5, 0.975),
                 prop_6_2.5 = quantile(prop_averted_6, 0.025),
                 prop_6_50 = quantile(prop_averted_6, 0.5),
                 prop_6_97.5 = quantile(prop_averted_6, 0.975),
                 prop_7_2.5 = quantile(prop_averted_7, 0.025),
                 prop_7_50 = quantile(prop_averted_7, 0.5),
                 prop_7_97.5 = quantile(prop_averted_7, 0.975)) 
high_dist <- join %>%
  dplyr::arrange(desc(prop_5_50)) %>%
  dplyr::slice_max(prop_5_50, n = 50) %>%
  dplyr::pull(district) 

prop <- full_join(join_no, join_smc) %>%
  full_join(join_smc_6) %>%
  full_join(join_smc_7) %>%
  dplyr::filter(district %in% high_dist) %>%
  dplyr::mutate(averted_5 = cases - cases_5,
                severe_5 = severe - severe_5,
                averted_6 = cases - cases_6,
                severe_6 = severe - severe_6,
                averted_7 = cases - cases_7,
                severe_7 = severe - severe_7,
                prop_averted_5 = (cases - cases_5)/cases,
                prop_averted_6 = (cases - cases_6)/cases,
                prop_averted_7 = (cases - cases_7)/cases) %>%
  dplyr::reframe(prop_5_2.5 = quantile(prop_averted_5, 0.025),
                 prop_5_50 = quantile(prop_averted_5, 0.5),
                 prop_5_97.5 = quantile(prop_averted_5, 0.975),
                 prop_6_2.5 = quantile(prop_averted_6, 0.025),
                 prop_6_50 = quantile(prop_averted_6, 0.5),
                 prop_6_97.5 = quantile(prop_averted_6, 0.975),
                 prop_7_2.5 = quantile(prop_averted_7, 0.025),
                 prop_7_50 = quantile(prop_averted_7, 0.5),
                 prop_7_97.5 = quantile(prop_averted_7, 0.975))

model_seasonality_func <- function(model_output, months = 5) {
  data <- model_output %>%
    dplyr::group_by(timestep) %>%
    dplyr::reframe(n_inc_clinical_1_1825 = median(n_inc_clinical_1_1825),
                   n_1_1825 = median(n_1_1825))
  
  years <- length(unique(data$timestep))/365
  len <- length(unique(data$timestep))
  annual_cases <- sum(data$n_inc_clinical_1_1825/data$n_1_1825,
                      na.rm = TRUE)/years
  days <- round(months * 30.25, digits = 0)
  
  prop <- numeric(0)
  for(i in 1:(len - (days-1))) {
    prop[i] <- (sum(data$n_inc_clinical_1_1825[i:(i+(days - 1))]/data$n_1_1825[i:(i+(days - 1))], 
                    na.rm = TRUE))/annual_cases
  }
  
  max <- max(prop)*100
  return(max)
  
}

# seasonality in each district by repetition
seasonality <- data.frame(district = character(0), repetitions = numeric(0), seasonality = numeric(0))
for(i in 1:length(districts)) {
  for(j in 1:20) {
    out <- no_smc %>%
      dplyr::filter(district == districts[i]) %>%
      dplyr::filter(repetition == j) %>%
      dplyr::filter(timestep < 366)
    metric <- model_seasonality_func(out)
    vec <- c(districts[i], j, metric)
    seasonality <- rbind(seasonality, vec)
  }
}
names(seasonality) <- c("district", "repetitions", "seasonality")
seasonality$seasonality <- as.numeric(seasonality$seasonality)
seasonality$district <- factor(seasonality$district)
seasonality$repetitions <- factor(seasonality$repetitions)
seasonality_summary <- seasonality %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(seasonality_2.5 = quantile(seasonality, 0.025),
                 seasonality_50 = quantile(seasonality),
                 seasonality_97.5 = quantile(seasonality, 0.975))

