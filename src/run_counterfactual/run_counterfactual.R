orderly2::orderly_strict_mode()
orderly2::orderly_resource("uga2.RDS")
orderly2::orderly_shared_resource("moz.rds")
orderly2::orderly_dependency(
  "demography",
  "latest",
  c(deathrates_matrix.RDS = "deathrates_matrix.RDS",
    ages.RDS = "ages.RDS"))
orderly2::orderly_artefact(description = "Counterfactual model run for Uganda", 
                           files = "df.RDS")
orderly2::orderly_parameters(repetitions = 20,
                             district = NULL,
                             country = NULL,
                             calibrated = NULL) 

if(calibrated == TRUE) {
  orderly2::orderly_dependency(
    "calibrate_eir",
    "latest(parameter:district == this:district)",
    c(calibrated_site.RDS = "calibrated_site.RDS"))
}

library(malariaEquilibrium)
library(malariasimulation)
library(tidyverse)

# demography
ages <- readRDS("ages.RDS")
deathrates_matrix <- readRDS("deathrates_matrix.RDS")

if((country %in% c("Mozambique", "Uganda")) == FALSE) {
  stop("Invalid country")
}

# Uganda site files
if(country == "Uganda") {
  if(calibrated == FALSE) {
    site <- readRDS("uga2.RDS")
  } else if(calibrated == TRUE) {
    site <- readRDS("calibrated_site.RDS")
  }
} else if(country == "Mozambique") {
  site <- readRDS("calibrated_site.RDS")
}

run_counterfactual <- function(population, # population size
                               sim_length, # simulation length
                               reps = 20, # number of repetitions
                               age_min, # lower bound on age bands for outputs
                               age_max, # upper bound on age bands for outputs
                               g0, # seasonality parameters
                               g1, g2, g3,
                               h1, h2, h3,
                               eir, # district EIR
                               deathrates_mat, # matrix of deathrates until demography is fixed) 
                               manipulate_cc = FALSE,
                               cc_matrix = NULL,
                               prop_perennial = NULL) 
{
  
  if (manipulate_cc == TRUE) {
    simparams <- get_parameters(
      list(
        human_population = population,
        clinical_incidence_rendering_min_ages = age_min,
        clinical_incidence_rendering_max_ages = age_max,
        severe_incidence_rendering_min_ages = age_min,
        severe_incidence_rendering_max_ages = age_max,
        prevalence_rendering_min_ages = age_min,
        prevalence_rendering_max_ages = age_max
      )
    )
    
    seasonal_funestus_params <- malariasimulation::fun_params
    seasonal_funestus_params$species <- "seasonal_funestus"
    
    perennial_funestus_params <- malariasimulation::fun_params
    perennial_funestus_params$species <- "perennial_funestus"
    
    vector_proportions <- c(1-prop_perennial, prop_perennial)
    
    simparams <- set_species(simparams, list(
      seasonal_funestus = seasonal_funestus_params,
      perennial_funestus = perennial_funestus_params),
      proportions = vector_proportions) |>
      set_carrying_capacity(
        timesteps = 1:(sim_length),
        carrying_capacity_scalers <- cc_matrix
      )  
  } else if (manipulate_cc == FALSE) {
    simparams <- get_parameters(
      list(
        human_population = population,
        model_seasonality = TRUE,
        g0 = g0,
        g = c(g1, g2, g3),
        h = c(h1, h2, h3),
        clinical_incidence_rendering_min_ages = age_min,
        clinical_incidence_rendering_max_ages = age_max,
        severe_incidence_rendering_min_ages = age_min,
        severe_incidence_rendering_max_ages = age_max,
        prevalence_rendering_min_ages = age_min,
        prevalence_rendering_max_ages = age_max
      )
    )
  }
  
  simparams <- set_equilibrium(simparams, eir)
  
  out <- run_simulation_with_repetitions(sim_length,
                                         repetitions = reps,
                                         overrides = simparams,
                                         parallel = TRUE)
  return(out)
}

if(country == "Uganda") {
  if(calibrated == FALSE) { # this has all sites whereas calibrated pulls in only one site 
    index <- which(site$seasonality$name_1 == district)
    # every site has a rural option - urban and rural have the same seasonality params so only eir needs filters
    
    params <- list(g0 = site$seasonality$g0[index],
                   g1 = site$seasonality$g1[index],
                   g2 = uga$seasonality$g2[index],
                   g3 = site$seasonality$g3[index],
                   h1 = site$seasonality$h1[index],
                   h2 = site$seasonality$h2[index],
                   h3 = site$seasonality$h3[index],
                   eir = site$eir$eir[site$eir$name_1 == district & 
                                        site$eir$urban_rural == "rural" &
                                        site$eir$spp == "pf"])
  } else if(calibrated == TRUE) {
    params <- list(g0 = site$seasonality$g0,
                   g1 = site$seasonality$g1,
                   g2 = site$seasonality$g2,
                   g3 = site$seasonality$g3,
                   h1 = site$seasonality$h1,
                   h2 = site$seasonality$h2,
                   h3 = site$seasonality$h3,
                   eir = site$eir$eir)
  }
} else if(country == "Mozambique") {
  params <- list(g0 = site$seasonality$seasonality_parameters$g0,
                 g1 = site$seasonality$seasonality_parameters$g1,
                 g2 = site$seasonality$seasonality_parameters$g2,
                 g3 = site$seasonality$seasonality_parameters$g3,
                 h1 = site$seasonality$seasonality_parameters$h1,
                 h2 = site$seasonality$seasonality_parameters$h2,
                 h3 = site$seasonality$seasonality_parameters$h3,
                 eir = site$eir$eir,
                 cc_matrix = as.matrix(site$seasonality$multiplier_matrix[,c(3,4)]), # needs to be a matrix otherwise causes error
                 prop_perennial = site$seasonality$proportion_perennial)
}

years <- 3
year <- 365
sim_length <- years * year
human_population <- 50000 # rescale in post processing to actual population size

age_min <- 1
age_max <- 5 * 365 # ages for SMC

if(country == "Uganda") {
  out <-  run_counterfactual(population = human_population,
                             sim_length = sim_length,
                             reps = repetitions,
                             g0 = params$g0,
                             g1 = params$g1,
                             g2 = params$g2,
                             g3 = params$g3,
                             h1 = params$h1,
                             h2 = params$h2,
                             h3 = params$h3,
                             eir = params$eir,
                             age_min = age_min,
                             age_max = age_max,
                             deathrates_mat = deathrates_matrix)
  # site files have different set ups
  scale <- max(site$population$pop[site$population$year == 2022]/human_population) 
} else if(country == "Mozambique") {
  out <-  run_counterfactual(population = human_population,
                             sim_length = sim_length,
                             reps = repetitions,
                             g0 = params$g0,
                             g1 = params$g1,
                             g2 = params$g2,
                             g3 = params$g3,
                             h1 = params$h1,
                             h2 = params$h2,
                             h3 = params$h3,
                             eir = params$eir,
                             age_min = age_min,
                             age_max = age_max,
                             deathrates_mat = deathrates_matrix,
                             manipulate_cc = TRUE,
                             cc_matrix = params$cc_matrix,
                             prop_perennial = params$prop_perennial)
  scale <- site$population$population_total |>
    dplyr::filter(urban_rural == "rural") |>
    dplyr::filter(year == 2022) |>
    dplyr::pull(pop)
  scale <- scale/human_population
}


# rescale so that this represents the actual population size of districts (rather than 25000 as is in the model sim)
out <- out %>% 
  dplyr::mutate(district = district,
                n_age_1_1825 = n_age_1_1825*scale,
                n_bitten = n_bitten * scale,
                n_inc_clinical_1_1825 = n_inc_clinical_1_1825 * scale,
                p_inc_clinical_1_1825 = p_inc_clinical_1_1825 * scale,
                n_inc_severe_1_1825 = n_inc_severe_1_1825 * scale,
                p_inc_severe_1_1825 = p_inc_severe_1_1825 * scale,
                n_detect_lm_1_1825 = n_detect_lm_1_1825 * scale,
                p_detect_lm_1_1825 = p_detect_lm_1_1825 * scale)

saveRDS(out, "df.RDS")


## comparing outputs - delete once comparison is completed
out_0.5 <- run_counterfactual(population = human_population,
                              sim_length = sim_length,
                              reps = 5,
                              g0 = params$g0,
                              g1 = params$g1,
                              g2 = params$g2,
                              g3 = params$g3,
                              h1 = params$h1,
                              h2 = params$h2,
                              h3 = params$h3,
                              eir = params$eir,
                              age_min = age_min,
                              age_max = age_max,
                              deathrates_mat = deathrates_matrix,
                              manipulate_cc = TRUE,
                              cc_matrix = params$cc_matrix,
                              prop_perennial = 0.5)
out_0.65 <- run_counterfactual(population = human_population,
                               sim_length = sim_length,
                               reps = 5,
                               g0 = params$g0,
                               g1 = params$g1,
                               g2 = params$g2,
                               g3 = params$g3,
                               h1 = params$h1,
                               h2 = params$h2,
                               h3 = params$h3,
                               eir = params$eir,
                               age_min = age_min,
                               age_max = age_max,
                               deathrates_mat = deathrates_matrix,
                               manipulate_cc = TRUE,
                               cc_matrix = params$cc_matrix,
                               prop_perennial = 0.65)
out_0.8 <- run_counterfactual(population = human_population,
                              sim_length = sim_length,
                              reps = 5,
                              g0 = params$g0,
                              g1 = params$g1,
                              g2 = params$g2,
                              g3 = params$g3,
                              h1 = params$h1,
                              h2 = params$h2,
                              h3 = params$h3,
                              eir = params$eir,
                              age_min = age_min,
                              age_max = age_max,
                              deathrates_mat = deathrates_matrix,
                              manipulate_cc = TRUE,
                              cc_matrix = params$cc_matrix,
                              prop_perennial = 0.8)
out_0.5$prop_perennial <- 0.5
out_0.65$prop_perennial <- 0.65
out_0.8$prop_perennial <- 0.8

out_compare <- rbind(out_0.5, out_0.65, out_0.8)
out_compare$prop_perennial <- factor(out_compare$prop_perennial)
out_summarised <- out_compare |>
  dplyr::group_by(timestep, prop_perennial) |>
  dplyr::reframe(incidence = median(n_inc_clinical_1_1825))

ggplot(out_summarised, aes(x = timestep, y = incidence, col = prop_perennial)) + 
  geom_line() + theme_bw() + scale_color_discrete()
ggsave("prop_perennial.pdf", dpi = 300, width = 25, height = 15, units = "cm")


out_1.0 <- run_counterfactual(population = human_population,
                              sim_length = sim_length,
                              reps = 5,
                              g0 = params$g0,
                              g1 = params$g1,
                              g2 = params$g2,
                              g3 = params$g3,
                              h1 = params$h1,
                              h2 = params$h2,
                              h3 = params$h3,
                              eir = params$eir,
                              age_min = age_min,
                              age_max = age_max,
                              deathrates_mat = deathrates_matrix,
                              manipulate_cc = TRUE,
                              cc_matrix = params$cc_matrix,
                              prop_perennial = 1.0)
ggplot(out_1.0, aes(x = timestep, y = n_inc_clinical_1_1825)) + theme_bw()
