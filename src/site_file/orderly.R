# orderly set up
orderly2::orderly_strict_mode()
orderly2::orderly_resource("data/UGA_spatial_test_data_1.RDS")
orderly2::orderly_resource("data/uga2.RDS")
orderly2::orderly_parameters(district = NULL)
# pull in the seasonality parameters from this district
orderly2::orderly_dependency("seasonality_parameters",
                             "latest(parameter:district == this:district) ",
                             c(params.RDS = "params.RDS")) 
# define the artefacts
orderly2::orderly_artefact(description = "District site file",
                           files = c("site_file.RDS"))

library(tidyverse)
library(Hmisc)

# new site file
uga_new <- readRDS("data/UGA_spatial_test_data_1.RDS")
# previous site file
uga2 <- readRDS("data/uga2.RDS")
# seasonality parameters 
params <- readRDS("params.RDS")

dist <- uga_new %>%
  dplyr::filter(name_2 == district) %>%
  dplyr::group_by(year, urban_rural) %>%
  dplyr::reframe(population = sum(pop),
                 par_pf = sum(par_pf),
                 par_pv = sum(par_pv),
                 prev = wtd.mean(pfpr, weights = pop),
                 itn_use = wtd.mean(pfpr, weights = pop),
                 irs_cov = wtd.mean(irs_cov, weights = pop),
                 tx_cov = wtd.mean(tx_cov, weights = pop),
                 gambiae_relative_abundance = wtd.mean(gambiae_relative_abundance, weights = pop), 
                 arabiensis_relative_abundance = wtd.mean(arabiensis_relative_abundance, weights = pop), 
                 funestus_relative_abundance = wtd.mean(funestus_relative_abundance, weights = pop))

# uga2

# site file has the following elements in the list:
# country, admin level, sites, cases_deaths (national), prevalence, interventions, population, 
# demography, vectors, pyrethroid resistance, seasonality, eir

# create tibbles in the list for the site file for this district
sites <- tibble(country = c("Uganda", "Uganda"),
                iso3c = c("UGA", "UGA"),
                name_1 = c(district, district),
                urban_rural = c("rural", "urban"))
cases_deaths <- uga2$cases_deaths
prevalence <- tibble(country = rep("Uganda", nrow(dist)),
                     iso3c = rep("UGA", nrow(dist)),
                     name_1 = rep(district, nrow(dist)),
                     urban_rural = dist$urban_rural,
                     year = dist$year,
                     pfpr = dist$prev,
                     pvpf = rep(0, nrow(dist)))

interventions <- tibble(country = rep("Uganda", nrow(dist)),
                        iso3c = rep("UGA", nrow(dist)),
                        name_1 = rep(district, nrow(dist)),
                        urban_rural = dist$urban_rural,
                        itn_use = dist$itn_use,
                        irs_cov = dist$irs_cov,
                        tx_cov = dist$tx_cov,
                        smc_cov = 0)

population <- tibble(country = rep("Uganda", nrow(dist)),
                     iso3c = rep("UGA", nrow(dist)),
                     name_1 = rep(district, nrow(dist)),
                     urban_rural = dist$urban_rural,
                     year = dist$year,
                     pop = dist$population,
                     par = dist$par_pf,
                     par_pf = dist$par_pf,
                     par_pv = 0)

demography <- NA
# unsure what to do about $ vectors

arabiensis <- dist %>%
  dplyr::filter(year %in% 2020:2023) %>%
  pull(arabiensis_relative_abundance) %>%
  median()
funestus <- dist %>%
  dplyr::filter(year %in% 2020:2023) %>%
  pull(funestus_relative_abundance) %>%
  median()
gambiae <- dist %>%
  dplyr::filter(year %in% 2020:2023) %>%
  pull(gambiae_relative_abundance) %>%
  median()
vec <- c(arabiensis, funestus, gambiae)
tot <- sum(vec)
vec <- vec/tot

vectors <- tibble(country = "Uganda",
                  iso3c = "UGA",
                  name_1 = district,
                  species = c("arabiensis", "funestus", "gambiae"),
                  prop = vec,
                  blood_meal_rates = 0.333,
                  foraging_time = 0.69,
                  Q0 = c(0.71, 0.94, 0.92),
                  phi_bednets = c(0.8,0.78,0.85),
                  phi_indoors = c(0.86,0.87,0.9),
                  mum = c(0.132, 0.112, 0.132))

# no pyrethroid resistance in Uganda
pyrethroid_resistance <- tibble(country = "Uganda",
                                iso3c = "UGA",
                                name_1 = district,
                                urban_rural = dist$urban_rural,
                                year = dist$year, 
                                pyrethroid_resistance = rep(0, length = nrow(dist)))

seasonality <- tibble(country = "Uganda",
                      iso3c = "UGA",
                      name_1 = district,
                      g0 = params$g0,
                      g1 = params$g1,
                      g2 = params$g2,
                      g3 = params$g3,
                      h1 = params$h1,
                      h2 = params$h2,
                      h3 = params$h3)
# country, admin level, sites, cases_deaths (national), prevalence, interventions, population, 
# demography, vectors, pyrethroid resistance, seasonality, eir
site_file <- list(country = "UGA",
                  admin_level = 1,
                  sites = sites,
                  cases_deaths = cases_deaths,
                  prevalence = prevalence,
                  interventions = interventions,
                  population = population,
                  # demography = demography,
                  vectors = vectors,
                  pyrethroid_resistance = pyrethroid_resistance,
                  seasonality = seasonality
                  ) #eir = eir [cali]

saveRDS(site_file, "site_file.RDS")
