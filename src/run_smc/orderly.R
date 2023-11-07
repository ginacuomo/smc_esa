# orderly2::orderly_strict_mode()
orderly2::orderly_resource("uga2.RDS")
orderly2::orderly_dependency(
  "demography",
  "latest",
  c(deathrates_matrix.RDS = "deathrates_matrix.RDS",
    ages.RDS = "ages.RDS"))
orderly2::orderly_artefact("SMC model run for Uganda", "df_smc.RDS")
orderly2::orderly_parameters(repetitions = 20)

# demography
ages <- readRDS("ages.RDS")
deathrates_matrix <- readRDS("deathrates_matrix.RDS")

# Uganda site files
uga <- readRDS("uga2.RDS")

# function to return the starting time for the SMC implementation
# will make it run slower but more reliable SMC timing
optimal_timing <- function(output) {
  out <- output %>% 
    dplyr::select(timestep, n_1_1825, n_inc_clinical_1_1825)
  out <- rbind(out, out) %>%
    dplyr::mutate(times = seq(1:(max(out$timestep)*2))) %>%
    dplyr::mutate(incidence = n_inc_clinical_1_1825/n_1_1825)
  
  total <- numeric(365)
  for(i in 1:365) {
    total[i] <- sum(out$incidence[seq(i, i+149)])
  }
  start <- which(total == max(total))
  return(start)
}

run_with_smc <- function(population, # population size
                         sim_length, # simulation length
                         reps = 20, # number of repititions
                         g0, # seasonality parameters
                         g1, g2, g3,
                         h1, h2, h3,
                         age_min, # lower bound on age bands for outputs
                         age_max, # upper bound on age bands for outputs
                         eir, # district EIR
                         deathrates_mat, # matrix of deathrates until demography is fixed) 
                         admin_days = c(0, 30, 60, 90, 120), # admin dates
                         alpha, # drug parameters | resistance
                         beta) {# drug parameters | resistance
  # same as previously
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
      severe_incidence_rendering_max_ages = age_max
    )
  )
  
  simparams <- set_demography(
    parameters = simparams,
    agegroups = ages,
    timesteps = 0,
    deathrates = deathrates_mat
  )
  
  simparams <- set_equilibrium(simparams, eir)
  test <- run_simulation(timesteps = 365, simparams)
  start <- optimal_timing(test)
  smc_dates <- rep((365 * seq(1, years-1, by = 1)), 
                   each = length(admin_days)) + start + rep(admin_days, 2)
  
  simparams <- set_drugs(parameters = simparams, 
                         list(SP_AQ_params))
  
  # add smc
  smcparams <- set_smc(
    simparams,
    drug = 1,
    timesteps = smc_dates,
    coverages = rep(.9, length(smc_dates)),
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

# looking at only the rural areas
indices <- which(uga$sites$urban_rural == "rural")
names <- uga$sites$name_1[indices]
# every site has a rural option - urban and rural have the same seasonality params so only eir needs filters

params <- list(g0 = uga$seasonality$g0,
               g1 = uga$seasonality$g1,
               g2 = uga$seasonality$g2,
               g3 = uga$seasonality$g3,
               h1 = uga$seasonality$h1,
               h2 = uga$seasonality$h2,
               h3 = uga$seasonality$h3,
               eir = uga$eir$eir[ #uga$eir$name_1 %in% names & ## unnecessary for now
                 uga$eir$urban_rural == "rural" &
                   uga$eir$spp == "pf"])

years <- 3
year <- 365
sim_length <- years * year
human_population <- 10000
age_min <- 1
age_max <- 5 * 365

# test with only three districts for now
n_dist <- length(names)

# now repeat for SMC params
df_smc <- matrix(NA, ncol = 37, nrow = 0)
for(i in 1:n_dist) {
  message(paste("district number", i, "named", names[i]))
  out <- run_with_smc(population = human_population,
                      sim_length = sim_length,
                      reps = 20, 
                      g0 = params$g0[i],
                      g1 = params$g1[i],
                      g2 = params$g2[i],
                      g3 = params$g3[i],
                      h1 = params$h1[i],
                      h2 = params$h2[i],
                      h3 = params$h3[i],
                      eir = params$eir[i],
                      age_min = age_min,
                      age_max = age_max,
                      deathrates_mat = deathrates_matrix,
                      alpha = 3.930956, # in final version, alpha and beta will also be in the list
                      beta = 30.38846)
  out$district <- names[i]
  colnames(df_smc) <- names(out)
  df_smc <- rbind(df_smc, out)
}

saveRDS(df_smc, "df_smc.RDS")
