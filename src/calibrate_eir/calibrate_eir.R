# orderly set up
orderly2::orderly_strict_mode()
orderly2::orderly_parameters(district = NULL, country = NULL)
# pull in the seasonality parameters from this district
orderly2::orderly_dependency("site_file",
                             "latest(parameter:district == this:district) ",
                             c(site_file.RDS = "site_file.RDS")) 
# orderly artefact
if(district != "Kampala" & country == "Uganda") {
  orderly2::orderly_artefact(description = "Visual check of calibration", 
                             file = "visual_check.png")
}
orderly2::orderly_artefact(description = "Calibrated site file", 
                           file = "calibrated_site.RDS") 

# load packages
library(cali)
library(malariasimulation)
library(ggplot2)
library(tidyverse)

# read in site file
site_file <- readRDS("site_file.RDS")

if((country %in% c("Mozambique", "Uganda")) == FALSE) {
  stop("Invalid country")
}

if(country == "Uganda") {
  # summarise prevalence 2_10 estimates
  annual_pfpr_summary <- function(x){
    year <- ceiling(x$timestep / 365)
    pfpr <- x$n_detect_lm_730_3650  / x$n_age_730_3650
    tapply(pfpr, year, mean)
  }
  
  target <- site_file$prevalence %>%
    dplyr::filter(urban_rural == "rural") %>%
    dplyr::filter(year %in% 2018:2020) %>% 
    dplyr::pull(pfpr)
  
  parameters <- get_parameters(list(human_population = 5000, individual_mosquitoes = FALSE,
                                    model_seasonality = TRUE, 
                                    g0 = site_file$seasonality$g0,
                                    g = c(site_file$seasonality$g1, 
                                          site_file$seasonality$g2, 
                                          site_file$seasonality$g3),
                                    h = c(site_file$seasonality$h1, 
                                          site_file$seasonality$h2, 
                                          site_file$seasonality$h3))) 
  parameters$timesteps <- 365 * 3
  
  set.seed(123)
  out <- calibrate(
    parameters = parameters,
    target = target,
    summary_function = annual_pfpr_summary,
    eq_prevalence = target[1]
  )
  parameters <- set_equilibrium(parameters, init_EIR = out)
  raw <- run_simulation(parameters$timesteps + 100, parameters = parameters)
  raw$pfpr <- raw$n_detect_lm_730_3650 / raw$n_age_730_3650
  
  ggplot() +
    geom_point(aes(x = 365 * (0:2 + 0.5), y = target), col = "dodgerblue", size = 4) + 
    geom_line(data = raw, aes(x = timestep, y = pfpr), col = "deeppink", linewidth = 1) +
    ylim(0, 1) +
    theme_bw()
  ggsave("visual_check.png")
  
  eir <- tibble(country = "Uganda",
                iso3c = "UGA",
                name_1 = district,
                urban_rural = "rural",
                spp = "pf",
                eir = out)
  calibrated_site <- list(country = "UGA",
                          admin_level = 1,
                          sites = site_file$sites,
                          cases_deaths = site_file$cases_deaths,
                          prevalence = site_file$prevalence,
                          interventions = site_file$interventions,
                          population = site_file$population,
                          # demography = demography,
                          vectors = site_file$vectors,
                          pyrethroid_resistance = site_file$pyrethroid_resistance,
                          seasonality = site_file$seasonality,
                          eir = eir)
  saveRDS(calibrated_site, "calibrated_site.RDS")
} else if(country == "Mozambique") {
  # file names will need fixing afterwards
  orderly2::orderly_resource("data/key_parameter_df.RDS") 
  orderly2::orderly_resource("data/cc_multiplier_df.RDS")
  # need to get this to look like a standard site file in order for the subsequent tasks to work
  dist_params <- readRDS("data/key_parameter_df.RDS") |> 
    dplyr::filter(district_gadm_rain == district)
  multiplier_matrix <- readRDS("data/cc_multiplier_df.rds") |> 
    dplyr::filter(district_gadm_rain == district)
  
  site_file$eir$eir <- dist_params$calibrated_eir
  
  site_file$seasonality$proportion_perennial <- dist_params$proportion_perennial
  site_file$seasonality$multiplier_matrix <- multiplier_matrix
  
  saveRDS(site_file, "calibrated_site.RDS")
}
