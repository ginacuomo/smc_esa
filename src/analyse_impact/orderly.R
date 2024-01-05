# update resources when the dependencies workflow is resolved
orderly2::orderly_strict_mode()
orderly2::orderly_dependency(
  "run_smc",
  "latest(parameter:district == this:district 
       && parameter:calibrated == this:calibrated 
       && parameter:repetitions == this:repetitions)",
  c(df_smc.RDS = "df_smc.RDS"))
orderly2::orderly_dependency(
  "run_counterfactual",
  "latest(parameter:district == this:district 
       && parameter:calibrated == this:calibrated 
       && parameter:repetitions == this:repetitions)",
  c(df.RDS = "df.RDS"))
orderly2::orderly_dependency(
  "calibrate_eir",
  "latest(parameter:district == this:district)",
  c(site.RDS = "calibrated_site.RDS"))
orderly2::orderly_parameters(district = NULL,
                             calibrated = NULL, 
                             repetitions = 20) 
# artefact of this analysis
orderly2::orderly_artefact("Summary of impact", "df_comb.RDS")

library(tidyverse)

df <- readRDS("df.RDS") # counterfactual
df_smc <- readRDS("df_smc.RDS") # SMC
site <- readRDS("site.RDS")

quantile_95 <- function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}

# analyse outputs
no_smc_summary <- df %>%
  dplyr::mutate(year = ceiling(timestep/365)) %>% 
  dplyr::filter(year > 1) %>% # omit the first year where no SMC
  dplyr::group_by(year, district, repetition) %>%
  dplyr::reframe(total_cases = sum(n_inc_clinical_1_1825),
                 total_severe = sum(n_inc_severe_1_1825),
                 total_incidence = sum(n_inc_clinical_1_1825/n_1_1825)) %>%
  dplyr::arrange(year, district, repetition)
smc_summary <- df_smc %>% 
  dplyr::mutate(year = ceiling(timestep/365)) %>%
  dplyr::filter(year > 1) %>% # omit the first year where no SMC
  dplyr::group_by(year, district, repetition) %>%
  dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                 total_incidence_smc = sum(n_inc_clinical_1_1825/n_1_1825)) %>%
  dplyr::arrange(year, district, repetition)

df_comb <- cbind(no_smc_summary, 
                 total_cases_smc = smc_summary$total_cases_smc, 
                 total_incidence_smc = smc_summary$total_incidence_smc) %>%
  as.data.frame() %>%
  # dplyr::full_join(no_smc_summary, smc_summary, 
                            # by = c("year", "district", "repetition")) %>% causes an error?!
  dplyr::mutate(cases_averted = total_cases - total_cases_smc,
                proportion_averted = ((total_cases - total_cases_smc)/total_cases)*100,
                per_child = total_incidence - total_incidence_smc) %>%
  dplyr::group_by(year, district) %>%
  dplyr::reframe(averted_2.5 = quantile_95(cases_averted)[1],
                 averted_50 = quantile_95(cases_averted)[2],
                 averted_97.5 = quantile_95(cases_averted)[3], 
                 proportion_2.5 = quantile_95(proportion_averted)[1],
                 proportion_50 = quantile_95(proportion_averted)[2],
                 proportion_97.5 = quantile_95(proportion_averted)[3],
                 per_child_2.5 = quantile_95(per_child)[1],
                 per_child_50 = quantile_95(per_child)[2],
                 per_child_97.5 = quantile_95(per_child)[3]) %>%
  dplyr::arrange(district, year)

saveRDS(df_comb, "df_comb.RDS")

