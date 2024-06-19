library(tidyverse)

districts <- readRDS("districts.RDS")

for(i in 1:length(districts)) {
  orderly2::orderly_run("seasonality_parameters", 
                        parameters = list(district = districts[i]),
                        echo = FALSE)
}
