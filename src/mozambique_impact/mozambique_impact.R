orderly2::orderly_parameters(calibrated = TRUE,
                             repetitions = 20,
                             coverage = 0.8)
# all the district information
orderly2::orderly_shared_resource("moz_districts.RDS")
orderly2::orderly_resource("functions.R")
source("functions.R")

# artefacts
orderly2::orderly_artefact(description = "Impact of 4 cycles of SMC by district", 
                           files = "impact.RDS")
orderly2::orderly_artefact(description = "Trajectories with all repetitions and clinical and severe outputs",
                           files = "trajectories.RDS")
orderly2::orderly_artefact(description = "Proxy of cost-effectiveness - doses adminstered per case averted",
                           files = "doses.RDS")

library(tidyverse)
library(data.table)

## pull in all of the dependencies 
districts <- readRDS("moz_districts.RDS") |>
  dplyr::pull(district_gadm_rain) |>
  unique()
country <- "Mozambique"

# pull in all of the summary impacts for plotting
output <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_comb.RDS")
  names(files) <- file.path("data", paste0("df_comb_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("analyse_impact",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:country == environment:country &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == this:repetitions)),
                                           files)
  dt <- readRDS(metadata$files$here)
  output <- rbind(output, dt, fill = T)
}
saveRDS(output, "impact.RDS")

# make a df of the trajectories too
# pull in all of the summary impacts for plotting
no_smc <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df.RDS")
  names(files) <- file.path("data", paste0("df_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("run_counterfactual",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:country == environment:country &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:repetitions == this:repetitions)),
                                           files)
  dt <- readRDS(metadata$files$here)
  no_smc <- rbind(no_smc, dt, fill = T)
}

smc <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_smc.RDS")
  cycles <- 4
  names(files) <- file.path("data", paste0("df_smc_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:country == environment:country &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:cycles == 4 &&
                                                          parameter:repetitions == this:repetitions &&
                                                          parameter:coverage == this:coverage)), #ensure this is runs using cov = 80%
                                           files)
  dt <- readRDS(metadata$files$here)
  smc <- rbind(smc, dt, fill = T)
}
saveRDS(no_smc, "no_smc.RDS")
saveRDS(smc, "smc.RDS")

# do I actually want this?
no_smc_trajectories <- no_smc |>
  dplyr::select(timestep, n_age_1_1825, n_inc_clinical_1_1825, n_inc_severe_1_1825, repetition, district) |>
  dplyr::group_by(timestep, repetition, district) |>
  dplyr::mutate(clinical_incidence = n_inc_clinical_1_1825/n_age_1_1825,
                severe_incidence = n_inc_severe_1_1825/n_age_1_1825,
                scenario = "counterfactual")
smc_trajectories <- smc |>
  dplyr::select(timestep, n_age_1_1825, n_inc_clinical_1_1825, n_inc_severe_1_1825, repetition, district) |>
  dplyr::group_by(timestep, repetition, district) |>
  dplyr::mutate(clinical_incidence = n_inc_clinical_1_1825/n_age_1_1825,
                severe_incidence = n_inc_severe_1_1825/n_age_1_1825,
                scenario = "SMC")

# create a dataframe allowing for the plotting of trajectories of different districts
trajectories <- rbind(no_smc_trajectories, smc_trajectories)
saveRDS(trajectories, "trajectories.RDS")

doses <- smc |>
  dplyr::filter(timestep == 1) |>
  dplyr::select(n_age_1_1825, district, repetition) |>
  dplyr::group_by(district) |>
  dplyr::reframe(under_5s = mean(n_age_1_1825)) |>
  dplyr::mutate(doses = under_5s * coverage * 4) |>
  dplyr::full_join(output) |>
  dplyr::group_by(district) |>
  dplyr::mutate(dose_per_clin = doses/clinical_averted_50,
                dose_per_sev = doses/severe_averted_50) |>
  dplyr::select(district, under_5s, doses, dose_per_clin, dose_per_sev) |>
  dplyr::rename(doses_adminstered = doses)
saveRDS(doses, "doses.RDS")

quantile_95 <- function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}

# get a dataframe with the median and CrI at all timesteps
q95 <- data.table()
for(i in districts) {
  reps <- 20
  sim_length <- 3*365
  
  smc_mat <- no_smc_mat <- 
    matrix(ncol = sim_length, nrow = reps)
  
  for(j in 1:reps) {
    no_smc_mat[j,] <- no_smc |>
      dplyr::filter(district == i) |>
      dplyr::filter(repetition == j) |>
      dplyr::mutate(inf = n_inc_clinical_1_1825) |> 
      pull(inf)
    smc_mat[j,] <- smc |>
      dplyr::filter(district == i) |>
      dplyr::filter(repetition == j) |>
      dplyr::mutate(inf = n_inc_clinical_1_1825) |> 
      pull(inf)
  }
  
  no_smc_q95 <- t(apply(no_smc_mat, 2, quantile_95)) |> as.data.frame()
  smc_q95 <- t(apply(smc_mat, 2, quantile_95)) |> as.data.frame()
  no_smc_q95$time <- seq(1:nrow(no_smc_q95))
  smc_q95$time <- seq(1:nrow(smc_q95))
  no_smc_q95$intervention <- "no SMC"
  smc_q95$intervention <- "SMC"
  combined_q95 <- rbind(no_smc_q95, smc_q95)
  combined_q95$intervention <- as.factor(combined_q95$intervention)
  combined_q95$district <- i
  q95 <- rbind(q95, combined_q95)
}
saveRDS(q95, "q95.RDS")

q95_per_child <- data.table()
for(i in districts) {
  reps <- 20
  sim_length <- 3*365
  
  smc_mat_per_child  <- no_smc_mat_per_child <- 
    matrix(ncol = sim_length, nrow = reps)
  
  for(j in 1:reps) {
    no_smc_mat_per_child[j,] <- no_smc |>
      dplyr::filter(district == i) |>
      dplyr::filter(repetition == j) |>
      dplyr::mutate(per_child = (n_inc_clinical_1_1825/n_age_1_1825)) |> 
      pull(per_child)
    smc_mat_per_child[j,] <- smc |>
      dplyr::filter(district == i) |>
      dplyr::filter(repetition == j) |>
      dplyr::mutate(per_child = (n_inc_clinical_1_1825/n_age_1_1825)) |> 
      pull(per_child)
  }
  
  no_smc_per_child_q95 <- t(apply(no_smc_mat_per_child, 2, quantile_95)) |> as.data.frame()
  smc_per_child_q95 <- t(apply(smc_mat_per_child, 2, quantile_95)) |> as.data.frame()
  no_smc_per_child_q95$time <- seq(1:nrow(no_smc_per_child_q95))
  smc_per_child_q95$time <- seq(1:nrow(smc_per_child_q95))
  no_smc_per_child_q95$intervention <- "no SMC"
  smc_per_child_q95$intervention <- "SMC"
  combined_per_child_q95 <- rbind(no_smc_per_child_q95, smc_per_child_q95)
  combined_per_child_q95$intervention <- as.factor(combined_per_child_q95$intervention)
  combined_per_child_q95$district <- i
  q95_per_child <- rbind(q95_per_child, combined_per_child_q95)
}
saveRDS(q95_per_child, "q95_per_child.RDS")

smc_0.5 <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_smc.RDS")
  cycles <- 4
  names(files) <- file.path("data", paste0("df_smc_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:country == environment:country &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:cycles == 4 &&
                                                          parameter:repetitions == 5 &&
                                                          parameter:coverage == 0.5)), 
                                           files)
  dt <- readRDS(metadata$files$here)
  smc_0.5 <- rbind(smc_0.5, dt, fill = T)
}
smc_0.65 <- data.table()
for(i in 1:length(districts)) {
  district <- districts[i] 
  files <- c("df_smc.RDS")
  cycles <- 4
  names(files) <- file.path("data", paste0("df_smc_", district, ".RDS"))
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:country == environment:country &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:cycles == 4 &&
                                                          parameter:repetitions == 5 &&
                                                          parameter:coverage == 0.65)), 
                                           files)
  dt <- readRDS(metadata$files$here)
  smc_0.65 <- rbind(smc_0.65, dt, fill = T)
}

smc_summary_0.5 <- smc_0.5 |> 
  dplyr::mutate(year = ceiling(timestep/365)) |>
  dplyr::filter(year > 2) |> # omit the first two years
  dplyr::group_by(year, district, repetition) |>
  dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                 total_severe_smc = sum(n_inc_severe_1_1825),
                 total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                 total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
  dplyr::arrange(year, district, repetition) |>
  dplyr::mutate(coverage = "50%")
smc_summary_0.65 <- smc_0.65 |> 
  dplyr::mutate(year = ceiling(timestep/365)) |>
  dplyr::filter(year > 2) |> # omit the first two years
  dplyr::group_by(year, district, repetition) |>
  dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                 total_severe_smc = sum(n_inc_severe_1_1825),
                 total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                 total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
  dplyr::arrange(year, district, repetition) |>
  dplyr::mutate(coverage = "65%")
smc_summary_0.8 <- smc |> 
  dplyr::mutate(year = ceiling(timestep/365)) |>
  dplyr::filter(repetition <= 5) |>
  dplyr::filter(year > 2) |> # omit the first two years
  dplyr::group_by(year, district, repetition) |>
  dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                 total_severe_smc = sum(n_inc_severe_1_1825),
                 total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                 total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
  dplyr::arrange(year, district, repetition) |>
  dplyr::mutate(coverage = "80%")
no_smc_summary <- no_smc |>
  dplyr::mutate(year = ceiling(timestep/365)) |> 
  dplyr::filter(year > 2) |> # omit the first year where no SMC
  dplyr::filter(repetition < 6) |>
  dplyr::group_by(year, district, repetition) |>
  dplyr::reframe(total_cases = sum(n_inc_clinical_1_1825),
                 total_severe = sum(n_inc_severe_1_1825),
                 total_incidence = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                 total_incidence_severe = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
  dplyr::arrange(year, district, repetition) |>
  dplyr::mutate(coverage = "0%")

## summarise impact
df_comb_0.5 <- smc_summary_0.5 |>
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
                 proportion_clinical_2.5 = quantile_95(proportion_averted)[1],
                 proportion_clinical_50 = quantile_95(proportion_averted)[2],
                 proportion_clinical_97.5 = quantile_95(proportion_averted)[3],
                 proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                 proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                 proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                 per_child_clinical_2.5 = quantile_95(per_child)[1],
                 per_child_clinical_50 = quantile_95(per_child)[2],
                 per_child_clinical_97.5 = quantile_95(per_child)[3],
                 per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                 per_child_severe_50 = quantile_95(per_child_severe)[2],
                 per_child_severe_97.5 = quantile_95(per_child_severe)[3]) |>
  dplyr::mutate(coverage_percent = "50%",
                coverage = 50)
df_comb_0.65 <- smc_summary_0.65 |>
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
                 proportion_clinical_2.5 = quantile_95(proportion_averted)[1],
                 proportion_clinical_50 = quantile_95(proportion_averted)[2],
                 proportion_clinical_97.5 = quantile_95(proportion_averted)[3],
                 proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                 proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                 proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                 per_child_clinical_2.5 = quantile_95(per_child)[1],
                 per_child_clinical_50 = quantile_95(per_child)[2],
                 per_child_clinical_97.5 = quantile_95(per_child)[3],
                 per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                 per_child_severe_50 = quantile_95(per_child_severe)[2],
                 per_child_severe_97.5 = quantile_95(per_child_severe)[3]) |>
  dplyr::mutate(coverage_percent = "65%",
                coverage = 65)
df_comb_0.8 <- smc_summary_0.8 |>
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
                 proportion_clinical_2.5 = quantile_95(proportion_averted)[1],
                 proportion_clinical_50 = quantile_95(proportion_averted)[2],
                 proportion_clinical_97.5 = quantile_95(proportion_averted)[3],
                 proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                 proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                 proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                 per_child_clinical_2.5 = quantile_95(per_child)[1],
                 per_child_clinical_50 = quantile_95(per_child)[2],
                 per_child_clinical_97.5 = quantile_95(per_child)[3],
                 per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                 per_child_severe_50 = quantile_95(per_child_severe)[2],
                 per_child_severe_97.5 = quantile_95(per_child_severe)[3]) |>
  dplyr::mutate(coverage_percent = "80%",
                coverage = 80)
df_comb <- rbind(df_comb_0.5, df_comb_0.65, df_comb_0.8)
baseline_incidence <- no_smc_summary |>
  dplyr::select(district, total_incidence, total_incidence_severe) |>
  dplyr::group_by(district) |>
  dplyr::reframe(clinical = median(total_incidence),
                 severe = median(total_incidence_severe)) 

coverage_impact <- left_join(df_comb, baseline_incidence) 
# from Matt - Max prop in 4 months from stl fits
max_prop <- readRDS("max_prop_df_stl_2017.RDS") |>
  dplyr::arrange(max_prop_stl) 
coverage_impact <- full_join(coverage_impact, max_prop, join_by(district == district)) |>
  dplyr::arrange(max_prop_stl) 
coverage_impact$district <- factor(coverage_impact$district,
                                      levels = max_prop$district)

# ggplot(coverage_impact, aes(x = clinical, y = proportion_clinical_50, col = coverage_percent)) +
#   geom_point() + theme_bw() 
ggplot(coverage_impact, aes(x = district, y = proportion_clinical_50, col = coverage_percent)) +
  geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "District", y = "Proportion of clinical cases \naverted with 4 cycles (%)") + 
  guides(col = guide_legend(title = "Coverage"))
ggsave("coverage_1.png", dpi = 300, width = 25, height = 12, units = "cm")

# ggplot(coverage_impact, aes(x = coverage, y = proportion_clinical_50, group = district)) + 
#   geom_line() + theme_bw() + expand_limits(y = 0)

###############################################################################
## look at the trajectories
traj_1 <- plot_trajectory(q95_df = q95_df, "Muembe")
traj_2 <- plot_trajectory(q95_df = q95_df, "Monapo" )
ggarrange(traj_1, traj_2, labels = c("A", "B"))
ggsave("trajectories.png", dpi = 300, width = 25, height = 12, units = "cm")
          
muembe <- data.table()
district <- "Muembe"
coverage_vals <- c(0.5, 0.65, 0.7, 0.75, 0.85, 0.9, 0.95, 0.97, 0.99, 1.00)
for(i in 1:length(coverage_vals)) {
  coverage <- coverage_vals[i] 
  files <- c("df_smc.RDS")
  names(files) <- file.path("data", paste0("df_smc_", district, "_", coverage, ".RDS"))
  
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:country == environment:country &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:cycles == 4 &&
                                                          parameter:repetitions == this:repetitions &&
                                                          parameter:coverage == environment:coverage)),
                                           files)
  dt <- readRDS(metadata$files$here)
  dt$coverage <- coverage
  muembe <- rbind(muembe, dt, fill = T)
  
}
monapo <- data.table()
district <- "Monapo"
coverage_vals <- c(0.5, 0.65, 0.7, 0.75, 0.85, 0.9, 0.95, 0.97, 0.99, 1.00)
for(i in 1:length(coverage_vals)) {
  coverage <- coverage_vals[i] 
  files <- c("df_smc.RDS")
  names(files) <- file.path("data", paste0("df_smc_", district, "_", coverage, ".RDS"))
  
  metadata <- orderly2::orderly_dependency("run_smc",
                                           quote(latest(parameter:district == environment:district &&
                                                          parameter:country == environment:country &&
                                                          parameter:calibrated == TRUE &&
                                                          parameter:cycles == 4 &&
                                                          parameter:repetitions == this:repetitions &&
                                                          parameter:coverage == environment:coverage)),
                                           files)
  dt <- readRDS(metadata$files$here)
  dt$coverage <- coverage
  monapo <- rbind(monapo, dt, fill = T)
  
}

muembe_summary <- muembe |>
  as.data.frame() |>
  dplyr::mutate(year = ceiling(timestep/365)) |>
  dplyr::filter(year > 2) |> # omit the first two years
  dplyr::group_by(year, district, repetition, coverage) |>
  dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                 total_severe_smc = sum(n_inc_severe_1_1825),
                 total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                 total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
  dplyr::arrange(year, district, repetition, coverage)
monapo_summary <- monapo |>
  as.data.frame() |>
  dplyr::mutate(year = ceiling(timestep/365)) |>
  dplyr::filter(year > 2) |> # omit the first two years
  dplyr::group_by(year, district, repetition, coverage) |>
  dplyr::reframe(total_cases_smc = sum(n_inc_clinical_1_1825),
                 total_severe_smc = sum(n_inc_severe_1_1825),
                 total_incidence_smc = sum(n_inc_clinical_1_1825/n_age_1_1825), 
                 total_incidence_severe_smc = sum(n_inc_severe_1_1825/n_age_1_1825)) |>
  dplyr::arrange(year, district, repetition, coverage)

muembe_no_smc <- no_smc_summary |>
  dplyr::filter(district == "Muembe") |>
  dplyr::filter(repetition < 6) |>
  dplyr::select(-coverage)
muembe_comb <- muembe_summary |>
  dplyr::select(district, repetition, coverage, total_cases_smc:total_incidence_severe_smc) |>
  dplyr::full_join(muembe_no_smc, by = c("district", "repetition")) |> # combine with the counterfactual
  dplyr::group_by(district, repetition, coverage) |> 
  dplyr::reframe(cases_averted = total_cases - total_cases_smc,
                 severe_averted = total_severe - total_severe_smc,
                 proportion_averted = ((total_cases - total_cases_smc)/total_cases)*100,
                 proportion_averted_severe = ((total_severe - total_severe_smc)/total_severe)*100,
                 per_child = total_incidence - total_incidence_smc,
                 per_child_severe = total_incidence_severe - total_incidence_severe_smc) |>
  dplyr::ungroup() |>
  dplyr::group_by(district, coverage) |>
  dplyr::reframe(clinical_averted_2.5 = quantile_95(cases_averted)[1],
                 clinical_averted_50 = quantile_95(cases_averted)[2],
                 clinical_averted_97.5 = quantile_95(cases_averted)[3], 
                 severe_averted_2.5 = quantile_95(severe_averted)[1],
                 severe_averted_50 = quantile_95(severe_averted)[2],
                 severe_averted_97.5 = quantile_95(severe_averted)[3], 
                 proportion_clinical_2.5 = quantile_95(proportion_averted)[1],
                 proportion_clinical_50 = quantile_95(proportion_averted)[2],
                 proportion_clinical_97.5 = quantile_95(proportion_averted)[3],
                 proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                 proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                 proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                 per_child_clinical_2.5 = quantile_95(per_child)[1],
                 per_child_clinical_50 = quantile_95(per_child)[2],
                 per_child_clinical_97.5 = quantile_95(per_child)[3],
                 per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                 per_child_severe_50 = quantile_95(per_child_severe)[2],
                 per_child_severe_97.5 = quantile_95(per_child_severe)[3]) 
monapo_no_smc <- no_smc_summary |>
  dplyr::filter(district == "Monapo") |>
  dplyr::filter(repetition < 6) |>
  dplyr::select(-coverage)
monapo_comb <- monapo_summary |>
  dplyr::select(district, repetition, coverage, total_cases_smc:total_incidence_severe_smc) |>
  dplyr::full_join(monapo_no_smc, by = c("district", "repetition")) |> # combine with the counterfactual
  dplyr::group_by(district, repetition, coverage) |> 
  dplyr::reframe(cases_averted = total_cases - total_cases_smc,
                 severe_averted = total_severe - total_severe_smc,
                 proportion_averted = ((total_cases - total_cases_smc)/total_cases)*100,
                 proportion_averted_severe = ((total_severe - total_severe_smc)/total_severe)*100,
                 per_child = total_incidence - total_incidence_smc,
                 per_child_severe = total_incidence_severe - total_incidence_severe_smc) |>
  dplyr::ungroup() |>
  dplyr::group_by(district, coverage) |>
  dplyr::reframe(clinical_averted_2.5 = quantile_95(cases_averted)[1],
                 clinical_averted_50 = quantile_95(cases_averted)[2],
                 clinical_averted_97.5 = quantile_95(cases_averted)[3], 
                 severe_averted_2.5 = quantile_95(severe_averted)[1],
                 severe_averted_50 = quantile_95(severe_averted)[2],
                 severe_averted_97.5 = quantile_95(severe_averted)[3], 
                 proportion_clinical_2.5 = quantile_95(proportion_averted)[1],
                 proportion_clinical_50 = quantile_95(proportion_averted)[2],
                 proportion_clinical_97.5 = quantile_95(proportion_averted)[3],
                 proportion_severe_2.5 = quantile_95(proportion_averted_severe)[1],
                 proportion_severe_50 = quantile_95(proportion_averted_severe)[2],
                 proportion_severe_97.5 = quantile_95(proportion_averted_severe)[3],
                 per_child_clinical_2.5 = quantile_95(per_child)[1],
                 per_child_clinical_50 = quantile_95(per_child)[2],
                 per_child_clinical_97.5 = quantile_95(per_child)[3],
                 per_child_severe_2.5 = quantile_95(per_child_severe)[1],
                 per_child_severe_50 = quantile_95(per_child_severe)[2],
                 per_child_severe_97.5 = quantile_95(per_child_severe)[3]) 

coverage_exploration <- rbind(muembe_comb, monapo_comb) |>
  dplyr::filter(coverage != 0.99)

ggplot(coverage_exploration, aes(x = coverage, y = proportion_clinical_50, col = district)) + 
  geom_point() + geom_smooth() + theme_bw() + 
  labs(x = "Coverage (%)", y = "Proportion of clinical cases averted annually") +
  expand_limits(y = 0)
ggsave("coverage_2.png", dpi = 300, width = 25, height = 12, units = "cm")
