# assuming you have already run the demography task
# creates site files and runs counterfactual and SMC for all districts in Uganda, calibrated to MAP prev

districts <- readRDS("karamoja.RDS")

for(i in 1:length(districts)) {
  district <- districts[i]
  # orderly2::orderly_run("seasonality_parameters",
  #                       parameters = list(district = district),
  #                       echo = FALSE)
  # 
  # # site file
  # orderly2::orderly_run("site_file",
  #                       parameters = list(district = district),
  #                       echo = FALSE)
  # # eir
  # orderly2::orderly_run("calibrate_eir",
  #                       parameters = list(district = district),
  #                       echo = FALSE)
  # counterfactual
  orderly2::orderly_run("run_counterfactual", parameters = list(district = district,
                                                                calibrated = TRUE,
                                                                repetitions = 20,
                                                                transmission_impact = TRUE),
                        echo = FALSE)
  # smc 
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     cycles = 4,
                                                     repetitions = 20,
                                                     extend_ages = FALSE,
                                                     transmission_impact = TRUE),
                        echo = FALSE)
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     cycles = 5,
                                                     repetitions = 20,
                                                     extend_ages = FALSE,
                                                     transmission_impact = TRUE),
                        echo = FALSE)
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     cycles = 6,
                                                     repetitions = 20,
                                                     extend_ages = FALSE,
                                                     transmission_impact = TRUE),
                        echo = FALSE)
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     cycles = 7,
                                                     repetitions = 20,
                                                     extend_ages = FALSE,
                                                     transmission_impact = TRUE),
                        echo = FALSE)
  # analyse impact
  # orderly2::orderly_run("analyse_impact", parameters = list(district = district,
  #                                                           calibrated = TRUE,
  #                                                           repetitions = 20),
  #                       echo = FALSE)
}
