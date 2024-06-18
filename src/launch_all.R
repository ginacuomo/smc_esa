# assuming you have already run the demography task
# creates site files and runs counterfactual and SMC for all districts in Uganda, calibrated to MAP prev

districts <- readRDS("districts.RDS")
districts <- districts[!districts == "Kampala"]

orderly2::orderly_run("demography")

for(i in 1:length(districts)) {
  orderly2::orderly_run("seasonality_parameters", 
                        parameters = list(district = districts[i]),
                        echo = FALSE)
}
# site files run for all
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("site_file",
                        parameters = list(district = district),
                        echo = FALSE) }
# calibrate district specific EIR and add this to the site file
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("calibrate_eir",
                        parameters = list(district = district),
                        echo = FALSE) }

for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("run_counterfactual", parameters = list(district = district,
                                                                calibrated = TRUE,
                                                                repetitions = 20),
                        echo = FALSE) }
# run smc for all numbers of cycles - 4 -> 7
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20,
                                                     cycles = 4),
                        echo = FALSE) 
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20,
                                                     cycles = 5),
                        echo = FALSE) 
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20,
                                                     cycles = 6),
                        echo = FALSE) 
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20,
                                                     cycles = 7),
                        echo = FALSE) 
  }

# need to extend this to include the various different values of cycles
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("analyse_impact", parameters = list(district = district,
                                                            calibrated = TRUE,
                                                            repetitions = 20),
                        echo = FALSE) 
}


orderly2::orderly_run("country_impact", parameters = list(calibrated = TRUE,
                                                      repetitions = 20))

orderly2::orderly_run("map_impact", parameters = list(calibrated = TRUE,
                                                      repetitions = 20))

