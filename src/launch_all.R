# assuming you have already run the demography task
# creates site files and runs counterfactual and SMC for all districts in Uganda, calibrated to MAP prev

districts <- readRDS("districts.RDS")
districts <- districts[!districts == "Kampala"]
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
for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20),
                        echo = FALSE) }

for(i in 1:length(districts)) {
  district <- districts[i]
  orderly2::orderly_run("analyse_impact", parameters = list(district = district,
                                                            calibrated = TRUE,
                                                            repetitions = 20),
                        echo = FALSE) }
