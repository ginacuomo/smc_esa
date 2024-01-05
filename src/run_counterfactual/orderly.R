orderly2::orderly_strict_mode()
orderly2::orderly_resource("uga2.RDS")
orderly2::orderly_dependency(
  "demography",
  "latest",
  c(deathrates_matrix.RDS = "deathrates_matrix.RDS",
    ages.RDS = "ages.RDS"))
orderly2::orderly_artefact("Counterfactual model run for Uganda", "df.RDS")
orderly2::orderly_parameters(repetitions = 20,
                             district = NULL,
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

# Uganda site files
if(calibrated == FALSE) {
  uga <- readRDS("uga2.RDS")
} else if(calibrated == TRUE) {
  uga <- readRDS("calibrated_site.RDS")
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
                               deathrates_mat) # matrix of deathrates until demography is fixed) 
{
  
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
  
  simparams <- set_demography(
    parameters = simparams,
    agegroups = ages,
    timesteps = 0,
    deathrates = deathrates_mat
  )
  
  simparams <- set_equilibrium(simparams, eir)
  
  out <- run_simulation_with_repetitions(sim_length,
                                         repetitions = reps,
                                         overrides = simparams,
                                         parallel = TRUE)
  return(out)
}

if(calibrated == FALSE) { # this has all sites whereas calibrated pulls in only one site 
  index <- which(uga$seasonality$name_1 == district)
  # every site has a rural option - urban and rural have the same seasonality params so only eir needs filters
  
  params <- list(g0 = uga$seasonality$g0[index],
                 g1 = uga$seasonality$g1[index],
                 g2 = uga$seasonality$g2[index],
                 g3 = uga$seasonality$g3[index],
                 h1 = uga$seasonality$h1[index],
                 h2 = uga$seasonality$h2[index],
                 h3 = uga$seasonality$h3[index],
                 eir = uga$eir$eir[uga$eir$name_1 == district & 
                   uga$eir$urban_rural == "rural" &
                     uga$eir$spp == "pf"])
} else if(calibrated == TRUE) {
  params <- list(g0 = uga$seasonality$g0,
                 g1 = uga$seasonality$g1,
                 g2 = uga$seasonality$g2,
                 g3 = uga$seasonality$g3,
                 h1 = uga$seasonality$h1,
                 h2 = uga$seasonality$h2,
                 h3 = uga$seasonality$h3,
                 eir = uga$eir$eir)
}

years <- 3
year <- 365
sim_length <- years * year
human_population <- 25000 # rescale in post processing to actual population size

age_min <- 1
age_max <- 5 * 365 # ages for SMC

out <- run_counterfactual(population = human_population,
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
scale <- uga$population$pop[uga$population$year == 2022]/25000

# rescale so that this represents the actual population size of districts (rather than 25000 as is in the model sim)
out <- out %>% 
  dplyr::mutate(district = district,
                n_1_1825 = n_1_1825*scale,
                n_bitten = n_bitten * scale,
                n_inc_clinical_1_1825 = n_inc_clinical_1_1825 * scale,
                p_inc_clinical_1_1825 = p_inc_clinical_1_1825 * scale,
                n_inc_severe_1_1825 = n_inc_severe_1_1825 * scale,
                p_inc_severe_1_1825 = p_inc_severe_1_1825 * scale,
                n_detect_1_1825 = n_detect_1_1825 * scale,
                p_detect_1_1825 = p_detect_1_1825 * scale)

saveRDS(out, "df.RDS")
