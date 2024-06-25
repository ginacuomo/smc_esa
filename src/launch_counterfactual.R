# read in all districts
districts <- readRDS("districts.RDS")
#only modelling rural areas at the moment therefore omit Kampala
index <- which(districts == "Kampala")
districts <- c(districts[1:index-1], districts[index+1, length(districts)])

for(i in 1:length(districts)) {
  orderly2::orderly_run("run_counterfactual", parameters = list(district = districts[i],
                                                                calibrated = TRUE,
                                                                repetitions = 20))
}
