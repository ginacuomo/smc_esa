orderly2::orderly_resource("uga2.RDS")
orderly2::orderly_artefact(description = "District seasonality parameters",
                           files = c("params.RDS"))
orderly2::orderly_description("Generates Fourier parameters for district")
orderly2::orderly_parameters(district = NULL)

library(tidyverse)

uga <- readRDS("uga2.RDS")

# where in the site file is the district specified
index <- which(uga$seasonality$name_1 == district)
params <- list(g0 = uga$seasonality$g0[index],
               g1 = uga$seasonality$g1[index],
               g2 = uga$seasonality$g2[index],
               g3 = uga$seasonality$g3[index],
               h1 = uga$seasonality$h1[index],
               h2 = uga$seasonality$h2[index],
               h3 = uga$seasonality$h3[index],
               eir = uga$eir$eir[which(uga$eir$name_1 == district &
                                   uga$eir$urban_rural == "rural" &
                                   uga$eir$spp == "pf")])
saveRDS(params, "params.RDS")
