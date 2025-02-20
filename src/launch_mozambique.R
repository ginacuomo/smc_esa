require(orderly2)
require(tidyverse)
require(malariasimulation)

## read Mozambique districts
districts <- readRDS("moz_districts.RDS") |>
  dplyr::pull(district_gadm_rain) |> # think I need to use this to avoid issues - check with Matt
  unique()
country <- "Mozambique"

# site file
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("site_file",
                        parameters = list(district = district,
                                          country = country),
                        echo = FALSE) 
}

# calibrate eir
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("calibrate_eir",
                        parameters = list(district = district,
                                          country = country),
                        echo = FALSE) 
}

# run_counterfactual
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("run_counterfactual",
                        parameters = list(district = district,
                                          country = country,
                                          calibrated = TRUE,
                                          repetitions = 20),
                        echo = FALSE) 
}

# run_smc
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("run_smc",
                        parameters = list(district = district,
                                          country = country,
                                          calibrated = TRUE,
                                          repetitions = 20,
                                          cycles = 4),
                        echo = FALSE) 
}
