orderly2::orderly_strict_mode()
orderly2::orderly_resource("uga2.RDS")
orderly2::orderly_dependency(
  "demography",
  "latest",
  c(deathrates_matrix.RDS = "deathrates_matrix.RDS",
    ages.RDS = "ages.RDS"))
orderly2::orderly_artefact("Counterfactual model run for Uganda", "df.RDS")
orderly2::orderly_parameters(repetitions = 20)

library(malariaEquilibrium)
library(malariasimulation)

# demography
ages <- readRDS("ages.RDS")
deathrates_matrix <- readRDS("deathrates_matrix.RDS")

# Uganda site files
uga <- readRDS("uga2.RDS")

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
df <- matrix(NA, ncol = 36, nrow = 0)
for(i in 1:n_dist) {
  message(paste("district", i, "of", n_dist, "- named", names[i]))
  out <- run_counterfactual(population = human_population,
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
                            deathrates_mat = deathrates_matrix)
  out$district <- names[i]
  colnames(df) <- names(out)
  df <- rbind(df, out)
}

saveRDS(df, "df.RDS")
