# assuming you have already run the demography task
# creates site files and runs counterfactual and SMC for all districts in Uganda, calibrated to MAP prev

districts <- readRDS("karamoja.RDS")

for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("seasonality_parameters",
                        parameters = list(district = district),
                        echo = FALSE)
  
  # site file
  orderly2::orderly_run("site_file",
                        parameters = list(district = district),
                        echo = FALSE)
  # eir
  orderly2::orderly_run("calibrate_eir",
                        parameters = list(district = district),
                        echo = FALSE)
  # counterfactual
  orderly2::orderly_run("run_counterfactual", parameters = list(district = district,
                                                                calibrated = TRUE,
                                                                repetitions = 20),
                        echo = FALSE)
  # smc 5 cycles
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20),
                        echo = FALSE)
  # analyse impact
  orderly2::orderly_run("analyse_impact", parameters = list(district = district,
                                                            calibrated = TRUE,
                                                            repetitions = 20),
                        echo = FALSE)
}
