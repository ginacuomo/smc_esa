orderly2::orderly_strict_mode()
orderly2::orderly_resource("uga2.RDS")
orderly2::orderly_shared_resource("moz.rds")
orderly2::orderly_dependency(
  "demography",
  "latest",
  c(deathrates_matrix.RDS = "deathrates_matrix.RDS",
    ages.RDS = "ages.RDS"))
orderly2::orderly_artefact(description = "SMC model run", 
                           files = "df_smc.RDS")

# need to integrate the changes looking at extending the age range and transmission impacts from main branch
# unsure why when I branched from main these edits were missed
orderly2::orderly_parameters(repetitions = 20,
                             district = NULL,
                             country = NULL,
                             cycles = NULL,
                             calibrated = NULL, 
                             coverage = 0.8) 


if(calibrated == TRUE) {
  orderly2::orderly_dependency(
    "calibrate_eir",
    quote(latest(parameter:district == environment:district &&
                   parameter:calibrated == TRUE &&
                   parameter:country == environment:country)),
    c(calibrated_site.RDS = "calibrated_site.RDS"))
}

# packages
library(malariaEquilibrium)
library(malariasimulation)
library(tidyverse)

# demography
ages <- readRDS("ages.RDS")
deathrates_matrix <- readRDS("deathrates_matrix.RDS")

if((country %in% c("Mozambique", "Uganda")) == FALSE) {
  stop("Invalid country")
}

# site files
if(country == "Uganda") {
  if(calibrated == FALSE) {
    site <- readRDS("uga2.RDS")
  } else if(calibrated == TRUE) {
    site <- readRDS("calibrated_site.RDS")
  }
} else if(country == "Mozambique") {
  site <- readRDS("calibrated_site.RDS")
}

# function to return the starting time for the SMC implementation
# will make it run slower but more reliable SMC timing
optimal_timing <- function(output, cycle) {
  out <- output %>% 
    dplyr::select(timestep, n_age_1_1825, n_inc_clinical_1_1825)
  out <- rbind(out, out) %>%
    dplyr::mutate(times = seq(1:(max(out$timestep)*2))) %>%
    dplyr::mutate(incidence = n_inc_clinical_1_1825/n_age_1_1825)
  
  total <- numeric(365)
  dur <- (cycle * 30)-1
  for(i in 1:365) {
    total[i] <- sum(out$incidence[seq(i, i+dur)])
  }
  start <- which(total == max(total))
  return(start)
}

run_smc <- function(population, # population size
                    sim_length, # simulation length
                    reps = 20, # number of repititions
                    g0, # seasonality parameters
                    g1, g2, g3,
                    h1, h2, h3,
                    age_min, # lower bound on age bands for outputs
                    age_max, # upper bound on age bands for outputs
                    eir, # district EIR
                    deathrates_mat, # matrix of deathrates until demography is fixed) 
                    manipulate_cc = FALSE,
                    cc_matrix = NULL,
                    prop_perennial = NULL,
                    cycle,
                    # admin_days = c(0, 30, 60, 90, 120), # admin dates
                    alpha, # drug parameters | resistance
                    beta) {# drug parameters | resistance
  # aligned with counterfactual
  if (manipulate_cc == TRUE) { # custom carrying capacity
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
  } else if (manipulate_cc == FALSE) { # cc driven by seasonality 
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
  test <- run_simulation(timesteps = 365, simparams)
  start <- optimal_timing(test, cycle = cycle)
  # admin days now depends on numbers of cycles
  admin_days <- seq(from = 0, by = 30, length.out = cycle)
  smc_dates <- rep((365 * seq(1, years-1, by = 1)), 
                   each = length(admin_days)) + start + rep(admin_days, 2)
  
  simparams <- set_drugs(parameters = simparams, 
                         list(SP_AQ_params))
  
  # add smc
  smcparams <- set_smc(
    simparams,
    drug = 1,
    timesteps = smc_dates,
    coverages = rep(coverage, length(smc_dates)),  # use the coverage parameter
    min_ages = rep(3 * 30, length(smc_dates)),
    max_ages = rep(5 * 365-1, length(smc_dates))
  )
  
  # use custom drug parameters from read in parameters
  smcparams$drug_prophylaxis_shape <- alpha
  smcparams$drug_prophylaxis_scale <- beta
  
  # output data
  out_smc <- run_simulation_with_repetitions(sim_length,
                                             repetitions = reps,
                                             overrides = smcparams,
                                             parallel = TRUE)
  return(out_smc)
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
  cc_matrix <- site$seasonality$multiplier_matrix |>
    dplyr::select(seasonal_vector_multipliers, perennial_vector_multipliers) |>
    as.matrix()
  params <- list(g0 = site$seasonality$seasonality_parameters$g0,
                 g1 = site$seasonality$seasonality_parameters$g1,
                 g2 = site$seasonality$seasonality_parameters$g2,
                 g3 = site$seasonality$seasonality_parameters$g3,
                 h1 = site$seasonality$seasonality_parameters$h1,
                 h2 = site$seasonality$seasonality_parameters$h2,
                 h3 = site$seasonality$seasonality_parameters$h3,
                 eir = site$eir$eir,
                 cc_matrix = cc_matrix, # needs to be a matrix otherwise causes error
                 prop_perennial = site$seasonality$proportion_perennial)
}

years <- 3
year <- 365
sim_length <- years * year
human_population <- 50000 # rescale in post processing to actual population size
age_min <- 1
age_max <- 5 * 365

if(country == "Uganda") {
  out <- run_smc(population = human_population,
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
                 cycle = cycles,
                 alpha = 3.930956, 
                 beta = 30.38846)
  # site files have different set ups
  scale <- max(site$population$pop[site$population$year == 2022]/human_population) 
} else if(country == "Mozambique") {
  out <-  run_smc(population = human_population,
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
                  prop_perennial = params$prop_perennial,
                  cycle = cycles,
                  alpha = 3.930956, 
                  beta = 30.38846)
  scale <- site$population$population_total |>
    dplyr::filter(urban_rural == "rural") |>
    dplyr::filter(year == 2022) |>
    dplyr::pull(pop)
  scale <- scale/human_population
}

# rescale so that this represents the actual population size of districts (rather than 50000 as is in the model sim)
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

saveRDS(out, "df_smc.RDS")
