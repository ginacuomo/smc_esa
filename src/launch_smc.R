# read in all districts
districts <- readRDS("districts.RDS")
districts <- districts[districts != "Kampala"]

for(i in 1:length(districts)) {
  orderly2::orderly_run("run_smc", parameters = list(district = districts[i],
                                                     calibrated = TRUE,
                                                     repetitions = 20))
}
