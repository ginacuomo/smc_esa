districts <- readRDS("districts.RDS")
districts <- districts[!districts == "Kampala"]

task_create_expr(orderly2::orderly_run("demography"))
task_create_expr(orderly2::orderly_run("merge_rasters"))

for(i in 1:length(districts)) {
  task_create_expr(orderly2::orderly_run("seasonality_parameters", 
                        parameters = list(district = districts[i]),
                        echo = FALSE))
}
# site files run for all
for(i in 1:length(districts)) {
  district <- districts[i]
  task_create_expr(orderly2::orderly_run("site_file",
                        parameters = list(district = district),
                        echo = FALSE)) 
  }
# calibrate district specific EIR and add this to the site file
for(i in 1:length(districts)) {
  district <- districts[i]
  task_create_expr(orderly2::orderly_run("calibrate_eir",
                        parameters = list(district = district),
                        echo = FALSE)) }

for(i in 1:length(districts)) {
  district <- districts[i]
  task_create_expr(orderly2::orderly_run("run_counterfactual", parameters = list(district = district,
                                                                calibrated = TRUE,
                                                                repetitions = 20),
                        echo = FALSE)) }
# run smc for all numbers of cycles - 4 -> 7
for(i in 1:length(districts)) {
  district <- districts[i]
  task_create_expr(orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20,
                                                     cycles = 4),
                        echo = FALSE)) 
  task_create_expr(orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20,
                                                     cycles = 5),
                        echo = FALSE)) 
  task_create_expr(orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20,
                                                     cycles = 6),
                        echo = FALSE) )
  task_create_expr(orderly2::orderly_run("run_smc", parameters = list(district = district,
                                                     calibrated = TRUE,
                                                     repetitions = 20,
                                                     cycles = 7),
                        echo = FALSE) )
}

# need to extend this to include the various different values of cycles
for(i in 1:length(districts)) {
  district <- districts[i]
  task_create_expr(orderly2::orderly_run("analyse_impact", parameters = list(district = district,
                                                            calibrated = TRUE,
                                                            repetitions = 20),
                        echo = FALSE) )
}

## fails here -- debug this task Caused by error:
# ! The value of 'district' from environment is not suitable as a lookup
# - while evaluating environment:district
# - within           latest(parameter:district == environment:district)
# --- unsure what line this is 
task_create_expr(orderly2::orderly_run("country_impact", parameters = list(calibrated = TRUE,
                                                          repetitions = 20)))

task_create_expr(orderly2::orderly_run("map_impact", parameters = list(calibrated = TRUE,
                                                      repetitions = 20)))

