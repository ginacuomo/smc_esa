orderly2::orderly_parameters(calibrated = TRUE,
                             repetitions = 20)
# all the district information
orderly2::orderly_shared_resource("moz_districts.RDS")
orderly2::orderly_resource("functions.R")
source("functions.R")

# artefacts
orderly2::orderly_artefact(description = "Impact of 4 cycles of SMC by district", 
                           files = "impact.RDS")
orderly2::orderly_artefact(description = "Trajectories with all repetitions and clinical and severe outputs",
                           files = "trajectories.RDS")

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
                                                          parameter:repetitions == this:repetitions)),
                                           files)
  dt <- readRDS(metadata$files$here)
  smc <- rbind(smc, dt, fill = T)
}

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

plot_trajectory(trajectories, "Muembe")
plot_trajectory(trajectories, "Moma")
plot_trajectory(trajectories, "Sanga")
