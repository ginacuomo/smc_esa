# update resources when the dependencies workflow is resolved
orderly2::orderly_strict_mode()
orderly2::orderly_parameters(repetitions = 20,
                             district = NULL,
                             country = NULL,
                             calibrated = NULL, 
                             coverage = 0.8) 

orderly2::orderly_dependency(
  "run_smc",
  "latest(parameter:district == this:district 
       && parameter:country == this:country
       && parameter:calibrated == this:calibrated 
       && parameter:repetitions == this:repetitions 
       && parameter:cycles == 4
       && parameter:coverage == this:coverage)",
  c(df_smc_4.RDS = "df_smc.RDS"))
# some dependencies are country specific - only Uganda sims contain multiple cycle comparisons
if (country == "Uganda") {
  orderly2::orderly_dependency(
    "run_smc",
    "latest(parameter:district == this:district 
       && parameter:country == this:country
       && parameter:calibrated == this:calibrated 
       && parameter:repetitions == this:repetitions 
       && parameter:cycles == 5)",
    c(df_smc_5.RDS = "df_smc.RDS"))
  orderly2::orderly_dependency(
    "run_smc",
    "latest(parameter:district == this:district 
       && parameter:country == this:country
       && parameter:calibrated == this:calibrated 
       && parameter:repetitions == this:repetitions 
       && parameter:cycles == 6)",
    c(df_smc_6.RDS = "df_smc.RDS"))
  orderly2::orderly_dependency(
    "run_smc",
    "latest(parameter:district == this:district 
       && parameter:country == this:country
       && parameter:calibrated == this:calibrated 
       && parameter:repetitions == this:repetitions 
       && parameter:cycles == 7)",
    c(df_smc_7.RDS = "df_smc.RDS"))
}
orderly2::orderly_dependency(
  "run_counterfactual",
  "latest(parameter:district == this:district 
       && parameter:country == this:country
       && parameter:calibrated == this:calibrated 
       && parameter:repetitions == this:repetitions)",
  c(df.RDS = "df.RDS"))
orderly2::orderly_dependency(
  "calibrate_eir",
  "latest(parameter:district == this:district 
       && parameter:country == this:country)",
  c(site.RDS = "calibrated_site.RDS"))

# artefact of this analysis
orderly2::orderly_artefact(description = "Summary of impact", 
                           files = "df_comb.RDS")
if(country == "Uganda") { # Uganda specific output
  orderly2::orderly_artefact(description = "Baseline counterfactual",
                             files = "baseline.RDS")
  orderly2::orderly_artefact(description = "Incremental benefit vs baseline.RDS",
                             files = "incremental.RDS")
}

library(tidyverse)

site <- readRDS("site.RDS")
df <- readRDS("df.RDS") # counterfactual - no SMC
df_smc_4 <- readRDS("df_smc_4.RDS") # SMC
if(country == "Uganda") { 
  df_smc_5 <- readRDS("df_smc_5.RDS")
  df_smc_6 <- readRDS("df_smc_6.RDS")
  df_smc_7 <- readRDS("df_smc_7.RDS") 
}

quantile_95 <- function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}

# analyse outputs - consistent throughout and alignment b/w no smc and smc dfs
no_smc_summary <- df |>
  dplyr::mutate(year = ceiling(timestep/365)) |> 
  dplyr::filter(year > 2) |> # omit the first year where no SMC
  dplyr::group_by(year, district, repetition) |>
  dplyr::reframe(total_cases = sum(n_inc_clinical_1_1825),
                 total_severe = sum(n_inc_severe_1_1825),
                 total_incidence = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                 total_incidence_severe = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
  dplyr::arrange(year, district, repetition) |>
  dplyr::mutate(cycles = 0)
smc_summary_4 <- df_smc_4 |> 
  dplyr::mutate(year = ceiling(timestep/365)) |>
  dplyr::filter(year > 2) |> # omit the first two years
  dplyr::group_by(year, district, repetition) |>
  dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                 total_severe_smc = sum(n_inc_severe_1_1825),
                 total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                 total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
  dplyr::arrange(year, district, repetition) |>
  dplyr::mutate(cycles = 4)
if(country == "Uganda") {
  smc_summary_5 <- df_smc_5 |> 
    dplyr::mutate(year = ceiling(timestep/365)) |>
    dplyr::filter(year > 2) |> # omit the first year where no SMC
    dplyr::group_by(year, district, repetition) |>
    dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                   total_severe_smc = sum(n_inc_severe_1_1825),
                   total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                   total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
    dplyr::arrange(year, district, repetition) |>
    dplyr::mutate(cycles = 5)
  smc_summary_6 <- df_smc_6 |> 
    dplyr::mutate(year = ceiling(timestep/365)) |>
    dplyr::filter(year > 2) |> # omit the first year where no SMC
    dplyr::group_by(year, district, repetition) |>
    dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                   total_severe_smc = sum(n_inc_severe_1_1825),
                   total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                   total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
    dplyr::arrange(year, district, repetition) |>
    dplyr::mutate(cycles = 6)
  smc_summary_7 <- df_smc_7 |> 
    dplyr::mutate(year = ceiling(timestep/365)) |>
    dplyr::filter(year > 2) |> # omit the first year where no SMC
    dplyr::group_by(year, district, repetition) |>
    dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                   total_severe_smc = sum(n_inc_severe_1_1825),
                   total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                   total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
    dplyr::arrange(year, district, repetition) |>
    dplyr::mutate(cycles = 7)
}

#####
df_comb_4 <- smc_summary_4 |>
  dplyr::select(district, repetition, total_cases_smc:total_incidence_severe_smc) |>
  dplyr::full_join(no_smc_summary, by = c("district", "repetition")) |> # combine with the counterfactual
  dplyr::group_by(district, repetition) |> 
  dplyr::reframe(cases_averted = total_cases - total_cases_smc,
                 severe_averted = total_severe - total_severe_smc,
                 proportion_averted = ((total_cases - total_cases_smc)/total_cases)*100,
                 proportion_averted_severe = ((total_severe - total_severe_smc)/total_severe)*100,
                 per_child = total_incidence - total_incidence_smc,
                 per_child_severe = total_incidence_severe - total_incidence_severe_smc) |>
  dplyr::ungroup() |>
  dplyr::group_by(district) |>
  dplyr::reframe(clinical_averted_2.5 = quantile_95(cases_averted)[1],
                 clinical_averted_50 = quantile_95(cases_averted)[2],
                 clinical_averted_97.5 = quantile_95(cases_averted)[3], 
                 severe_averted_2.5 = quantile_95(severe_averted)[1],
                 severe_averted_50 = quantile_95(severe_averted)[2],
                 severe_averted_97.5 = quantile_95(severe_averted)[3], 
                 proportion_2.5 = quantile_95(proportion_averted)[1],
                 proportion_50 = quantile_95(proportion_averted)[2],
                 proportion_97.5 = quantile_95(proportion_averted)[3],
                 proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                 proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                 proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                 per_child_2.5 = quantile_95(per_child)[1],
                 per_child_50 = quantile_95(per_child)[2],
                 per_child_97.5 = quantile_95(per_child)[3],
                 per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                 per_child_severe_50 = quantile_95(per_child_severe)[2],
                 per_child_severe_97.5 = quantile_95(per_child_severe)[3]) |>
  dplyr::mutate(cycles = "4 cycles")
df_comb <- df_comb_4 |> 
  dplyr::arrange(district, cycles)
if(country == "Uganda") {
  df_comb_5 <- smc_summary_5 |>
    dplyr::select(district, repetition, total_cases_smc:total_incidence_severe_smc) |>
    dplyr::full_join(no_smc_summary, by = c("district", "repetition")) |> # combine with the counterfactual
    dplyr::group_by(district, repetition) |> 
    dplyr::reframe(cases_averted = total_cases - total_cases_smc,
                   severe_averted = total_severe - total_severe_smc,
                   proportion_averted = ((total_cases - total_cases_smc)/total_cases)*100,
                   proportion_averted_severe = ((total_severe - total_severe_smc)/total_severe)*100,
                   per_child = total_incidence - total_incidence_smc,
                   per_child_severe = total_incidence_severe - total_incidence_severe_smc) |>
    dplyr::ungroup() |>
    dplyr::group_by(district) |>
    dplyr::reframe(averted_2.5 = quantile_95(cases_averted)[1],
                   averted_50 = quantile_95(cases_averted)[2],
                   averted_97.5 = quantile_95(cases_averted)[3], 
                   severe_2.5 = quantile_95(severe_averted)[1],
                   severe_50 = quantile_95(severe_averted)[2],
                   severe_97.5 = quantile_95(severe_averted)[3], 
                   proportion_2.5 = quantile_95(proportion_averted)[1],
                   proportion_50 = quantile_95(proportion_averted)[2],
                   proportion_97.5 = quantile_95(proportion_averted)[3],
                   proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                   proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                   proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                   per_child_2.5 = quantile_95(per_child)[1],
                   per_child_50 = quantile_95(per_child)[2],
                   per_child_97.5 = quantile_95(per_child)[3],
                   per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                   per_child_severe_50 = quantile_95(per_child_severe)[2],
                   per_child_severe_97.5 = quantile_95(per_child_severe)[3]) |>
    dplyr::mutate(cycles = "5 cycles")
  df_comb_6 <- smc_summary_6 |>
    dplyr::select(district, repetition, total_cases_smc:total_incidence_severe_smc) |>
    dplyr::full_join(no_smc_summary, by = c("district", "repetition")) |> # combine with the counterfactual
    dplyr::group_by(district, repetition) |> 
    dplyr::reframe(cases_averted = total_cases - total_cases_smc,
                   severe_averted = total_severe - total_severe_smc,
                   proportion_averted = ((total_cases - total_cases_smc)/total_cases)*100,
                   proportion_averted_severe = ((total_severe - total_severe_smc)/total_severe)*100,
                   per_child = total_incidence - total_incidence_smc,
                   per_child_severe = total_incidence_severe - total_incidence_severe_smc) |>
    dplyr::ungroup() |>
    dplyr::group_by(district) |>
    dplyr::reframe(averted_2.5 = quantile_95(cases_averted)[1],
                   averted_50 = quantile_95(cases_averted)[2],
                   averted_97.5 = quantile_95(cases_averted)[3], 
                   severe_2.5 = quantile_95(severe_averted)[1],
                   severe_50 = quantile_95(severe_averted)[2],
                   severe_97.5 = quantile_95(severe_averted)[3], 
                   proportion_2.5 = quantile_95(proportion_averted)[1],
                   proportion_50 = quantile_95(proportion_averted)[2],
                   proportion_97.5 = quantile_95(proportion_averted)[3],
                   proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                   proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                   proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                   per_child_2.5 = quantile_95(per_child)[1],
                   per_child_50 = quantile_95(per_child)[2],
                   per_child_97.5 = quantile_95(per_child)[3],
                   per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                   per_child_severe_50 = quantile_95(per_child_severe)[2],
                   per_child_severe_97.5 = quantile_95(per_child_severe)[3]) |>
    dplyr::mutate(cycles = "6 cycles")
  df_comb_7 <- smc_summary_7 |>
    dplyr::select(district, repetition, total_cases_smc:total_incidence_severe_smc) |>
    dplyr::full_join(no_smc_summary, by = c("district", "repetition")) |> # combine with the counterfactual
    dplyr::group_by(district, repetition) |> 
    dplyr::reframe(cases_averted = total_cases - total_cases_smc,
                   severe_averted = total_severe - total_severe_smc,
                   proportion_averted = ((total_cases - total_cases_smc)/total_cases)*100,
                   proportion_averted_severe = ((total_severe - total_severe_smc)/total_severe)*100,
                   per_child = total_incidence - total_incidence_smc,
                   per_child_severe = total_incidence_severe - total_incidence_severe_smc) |>
    dplyr::ungroup() |>
    dplyr::group_by(district) |>
    dplyr::reframe(averted_2.5 = quantile_95(cases_averted)[1],
                   averted_50 = quantile_95(cases_averted)[2],
                   averted_97.5 = quantile_95(cases_averted)[3], 
                   severe_2.5 = quantile_95(severe_averted)[1],
                   severe_50 = quantile_95(severe_averted)[2],
                   severe_97.5 = quantile_95(severe_averted)[3], 
                   proportion_2.5 = quantile_95(proportion_averted)[1],
                   proportion_50 = quantile_95(proportion_averted)[2],
                   proportion_97.5 = quantile_95(proportion_averted)[3],
                   proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                   proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                   proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                   per_child_2.5 = quantile_95(per_child)[1],
                   per_child_50 = quantile_95(per_child)[2],
                   per_child_97.5 = quantile_95(per_child)[3],
                   per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                   per_child_severe_50 = quantile_95(per_child_severe)[2],
                   per_child_severe_97.5 = quantile_95(per_child_severe)[3]) |>
    dplyr::mutate(cycles = "7 cycles")
  df_comb <- rbind(df_comb_4, df_comb_5, df_comb_6, df_comb_7) |>
    dplyr::arrange(district, cycles)
}

saveRDS(df_comb, "df_comb.RDS")


# now looking at the incremental benefit of additional cycles 

if(country == "Uganda") {
  # baseline effect
  saveRDS(df_comb_4, "baseline.RDS")
  
  df_smc_4$cycles <- "4 cycles"
  df_smc_5$cycles <- "5 cycles"
  df_smc_6$cycles <- "6 cycles"
  df_smc_7$cycles <- "7 cycles"
  
  ## now need differences between 4 and additional cycles
  incremental_5 <- rbind(df_smc_4, df_smc_5) |>
    dplyr::select(timestep, n_age_1_1825, n_inc_clinical_1_1825, n_inc_severe_1_1825, 
                  repetition, district, cycles) |>
    dplyr::filter(timestep > 730) |>
    dplyr::group_by(cycles, repetition) |>
    dplyr::reframe(cases = sum(n_inc_clinical_1_1825), 
                   severe = sum(n_inc_severe_1_1825), 
                   incidence = sum(n_inc_clinical_1_1825/n_age_1_1825)) |>
    dplyr::group_by(repetition) |>
    dplyr::reframe(cases_averted = cases - lead(cases, default = cases[2]),
                   severe_averted = severe - lead(severe, default = severe[2]),
                   incidence_averted = incidence - lead(incidence, default = incidence[2])) |>
    dplyr::filter(cases_averted != 0) |>
    dplyr::ungroup() |>
    dplyr::reframe(averted_2.5 = quantile_95(cases_averted)[1],
                   averted_50 = quantile_95(cases_averted)[2],
                   averted_97.5 = quantile_95(cases_averted)[3], 
                   severe_2.5 = quantile_95(severe_averted)[1],
                   severe_50 = quantile_95(severe_averted)[2],
                   severe_97.5 = quantile_95(severe_averted)[3],
                   per_child_2.5 = quantile_95(incidence_averted)[1],
                   per_child_50 = quantile_95(incidence_averted)[2],
                   per_child_97.5 = quantile_95(incidence_averted)[3]) |>
    dplyr::mutate(cycles = "5 cycles")
  
  incremental_6 <- rbind(df_smc_4, df_smc_6) |>
    dplyr::select(timestep, n_age_1_1825, n_inc_clinical_1_1825, n_inc_severe_1_1825, 
                  repetition, district, cycles) |>
    dplyr::filter(timestep > 730) |>
    dplyr::group_by(cycles, repetition) |>
    dplyr::reframe(cases = sum(n_inc_clinical_1_1825), 
                   severe = sum(n_inc_severe_1_1825), 
                   incidence = sum(n_inc_clinical_1_1825/n_age_1_1825)) |>
    dplyr::group_by(repetition) |>
    dplyr::reframe(cases_averted = cases - lead(cases, default = cases[2]),
                   severe_averted = severe - lead(severe, default = severe[2]),
                   incidence_averted = incidence - lead(incidence, default = incidence[2])) |>
    dplyr::filter(cases_averted != 0) |>
    dplyr::ungroup() |>
    dplyr::reframe(averted_2.5 = quantile_95(cases_averted)[1],
                   averted_50 = quantile_95(cases_averted)[2],
                   averted_97.5 = quantile_95(cases_averted)[3], 
                   severe_2.5 = quantile_95(severe_averted)[1],
                   severe_50 = quantile_95(severe_averted)[2],
                   severe_97.5 = quantile_95(severe_averted)[3],
                   per_child_2.5 = quantile_95(incidence_averted)[1],
                   per_child_50 = quantile_95(incidence_averted)[2],
                   per_child_97.5 = quantile_95(incidence_averted)[3]) |>
    dplyr::mutate(cycles = "6 cycles")
  
  incremental_7 <- rbind(df_smc_4, df_smc_7) |>
    dplyr::select(timestep, n_age_1_1825, n_inc_clinical_1_1825, n_inc_severe_1_1825, 
                  repetition, district, cycles) |>
    dplyr::filter(timestep > 730) |>
    dplyr::group_by(cycles, repetition) |>
    dplyr::reframe(cases = sum(n_inc_clinical_1_1825), 
                   severe = sum(n_inc_severe_1_1825), 
                   incidence = sum(n_inc_clinical_1_1825/n_age_1_1825)) |>
    dplyr::group_by(repetition) |>
    dplyr::reframe(cases_averted = cases - lead(cases, default = cases[2]),
                   severe_averted = severe - lead(severe, default = severe[2]),
                   incidence_averted = incidence - lead(incidence, default = incidence[2])) |>
    dplyr::filter(cases_averted != 0) |>
    dplyr::ungroup() |>
    dplyr::reframe(averted_2.5 = quantile_95(cases_averted)[1],
                   averted_50 = quantile_95(cases_averted)[2],
                   averted_97.5 = quantile_95(cases_averted)[3], 
                   severe_2.5 = quantile_95(severe_averted)[1],
                   severe_50 = quantile_95(severe_averted)[2],
                   severe_97.5 = quantile_95(severe_averted)[3],
                   per_child_2.5 = quantile_95(incidence_averted)[1],
                   per_child_50 = quantile_95(incidence_averted)[2],
                   per_child_97.5 = quantile_95(incidence_averted)[3]) |>
    dplyr::mutate(cycles = "7 cycles")
  
  incremental <- rbind(incremental_5, incremental_6, incremental_7)
  saveRDS(incremental, "incremental.RDS")
}
