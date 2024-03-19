orderly2::orderly_strict_mode()
orderly2::orderly_resource("ssa_demography_2021.csv")
orderly2::orderly_artefact(description = "Demography matrix", 
                           files = c("deathrates_matrix.RDS"))
orderly2::orderly_artefact(description = "Age breaks for model", 
                           files = c("ages.RDS"))

# fix demography
demog <- read.csv("ssa_demography_2021.csv")
# Age group upper
ages <- round(demog$age_upper * 365)
# Rescale the deathrates to be on the daily timestep
rescale_prob <- function(p, interval_in, interval_out){
  1 - (1 - p) ^ (interval_out / interval_in)
}
deathrates <- rescale_prob(demog$mortality_rate, 365, 1)
# Create matrix of death rates
deathrates_matrix <- matrix(deathrates, nrow = length(1), byrow = TRUE)

saveRDS(deathrates_matrix, "deathrates_matrix.RDS")
saveRDS(ages, "ages.RDS")
