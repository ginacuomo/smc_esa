library(tidyverse)

districts <- readRDS("seasonality_parameters/input/districts.rds")

for(i in 1:length(districts)) {
  orderly2::orderly_run("site_file", 
                        parameters = list(district = districts[i]),
                        echo = FALSE)
}
