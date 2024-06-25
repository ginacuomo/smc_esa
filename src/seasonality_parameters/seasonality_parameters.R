orderly2::orderly_dependency(
  "merge_rasters",
  "latest()",
  c(full_data.RDS = "full_data.RDS"))
orderly2::orderly_artefact(description = "District seasonality parameters",
                           files = c("params.RDS"))
orderly2::orderly_description("Generates Fourier parameters for district")
orderly2::orderly_parameters(district = NULL)

library(tidyverse)
library(ggplot2)

full_data <- readRDS("full_data.RDS")

# make df long
data <- full_data %>%
  tidyr::pivot_longer(cols = raster1:raster365, names_to = "day", values_to = "rainfall") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(day = as.numeric(unlist(strsplit(day, "raster"))[2])) %>%
  dplyr::group_by(day, districts) %>%
  dplyr::reframe(rainfall = sum(rainfall)) %>%
  dplyr::arrange(districts)

## fit seasonality parameters
df <- dplyr::filter(data, districts == district)
params <- umbrella::fit_fourier(rainfall = df$rainfall, t = df$day, floor = 0.5)
predict <- umbrella::fourier_predict(coef = params$coefficients, t = 1:365, 
                                     floor = params$floor)

ggplot() + geom_point(data = df, aes(x = day, y = rainfall)) + 
  geom_line(data = predict, aes(x = t, y = profile)) + theme_bw()

# where in the site file is the district specified
params <- list(g0 = params$coefficients["g0"],
               g1 = params$coefficients["g1"],
               g2 = params$coefficients["g2"],
               g3 = params$coefficients["g3"],
               h1 = params$coefficients["h1"],
               h2 = params$coefficients["h2"],
               h3 = params$coefficients["h3"])

saveRDS(params, "params.RDS")
