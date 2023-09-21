# basic implementation 
# steps
# create dataset of site files
# create function that runs the model
# calibrate model
# update parameters
# run with the intervention
# assess impact

library(malariaEquilibrium)
library(malariasimulation)

# fix demography
demog <- read.csv("data/ssa_demography_2021.csv")
# Age group upper
ages <- round(demog$age_upper * 365)
# Rescale the deathrates to be on the daily timestep
rescale_prob <- function(p, interval_in, interval_out){
  1 - (1 - p) ^ (interval_out / interval_in)
}
deathrates <- rescale_prob(demog$mortality_rate, 365, 1)
# Create matrix of death rates
deathrates_matrix <- matrix(deathrates, nrow = length(1), byrow = TRUE)

# Uganda site files
uga <- readRDS("data/uga2.RDS")

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
  
  out <- run_simulation_with_repetitions(sim_length,
                                         repetitions = reps,
                                         overrides = simparams,
                                         parallel = TRUE)
  return(out)
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
                         admin_days = c(-75, -45, -15, 15, 45), # admin dates
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
  simparams <- set_drugs(parameters = simparams, 
                            list(SP_AQ_params))
  
  peak <- peak_season_offset(simparams)
  
  smc_dates <- rep((365 * seq(1, years-1, by = 1)), 
                  each = length(admin_days)) + peak + rep(admin_days, 2)
  
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

## testing based upon UGA site files
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
n_dist <- 3
df <- matrix(NA, ncol = 36, nrow = 0)
for(i in 1:n_dist) {
  out <- run_counterfactual(population = human_population,
                            sim_length = sim_length,
                            reps = 2, 
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
                            deathrates_mat = deathrates_matrix) 
  out$district <- names[i]
  colnames(df) <- names(out)
  df <- rbind(df, out)
}

# now repeat for SMC params
df_smc <- matrix(NA, ncol = 37, nrow = 0)
for(i in 1:n_dist) {
  out <- run_with_smc(population = human_population,
                      sim_length = sim_length,
                      reps = 2, 
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

# now write code to compare the impact of SMC in each region

#'input: df and df_smc
#' process: estimate key variables in each district such as number of cases total, number of cases in 
# children under 5, number of SMC doses delivered, prevalence and then estimate %age of cases averted annually
# following SMC implementation








