orderly2::orderly_dependency(
  "run_counterfactual",
  "latest",
  c(model_out.RDS = "df.RDS"))
orderly2::orderly_resource("uganda-jan-21-feb-22.csv")
orderly2::orderly_resource("uganda-march-22-dec-22.csv")

library(ggplot2)
library(tidyverse)
library(lubridate)

uga1 <- read.csv("uganda-jan-21-feb-22.csv", header = FALSE)
uga2 <- read.csv("uganda-march-22-dec-22.csv", header = FALSE)
names(uga1) <- uga1[1,]
names(uga2) <- uga2[1,]
index1 <- which(names(uga1) != "NA")
index2 <- which(names(uga2) != "NA")
uga1 <- uga1[2:nrow(uga1),index1]
uga2 <- uga2[2:nrow(uga2),index2]
uga <- dplyr::full_join(uga1, uga2, by = c("Region", "District",
                                           "Subcounty", "Facility"))
df <-  uga[,grepl("Malaria Confirmed", names(uga))]
uga <- cbind(data.frame(uga[,c("Region", "District", "Subcounty", "Facility")]), df)
start <- names(uga)[5]
end <- names(uga)[ncol(uga)]
karamoja <- uga %>%
  tidyr::pivot_longer(cols = paste(start):paste(end),
                      names_to = "variable",
                      values_to = "confirmed_cases") %>%
  dplyr::mutate(month = str_split_i(variable, pattern = " ", 1),
                year = str_split_i(variable, pattern = " ", 2),
                age_group = str_split_i(variable, pattern = " ", 10),
                sex = str_split_i(variable, pattern = " ", 11)) %>%
  dplyr::mutate(sex = if_else(grepl("Female", sex), "Female", "Male")) %>% # fix sex variables
  dplyr::mutate(date = lubridate::my(paste0(month, "/", year))) %>% # fix date
  dplyr::mutate(age_group = gsub(',','', age_group)) %>%
  dplyr::mutate(District = gsub(" ", "", District)) %>%
  dplyr::select(-c("Region","variable", "month", "year")) 

karamoja$confirmed_cases <- as.numeric(karamoja$confirmed_cases)
unique(karamoja$age_group)
karamoja$age_group <- factor(karamoja$age_group, 
                             levels = c("0-28Dys", 
                                        "29Dys-4Yrs", 
                                        "5-9Yrs", 
                                        "10-19Yrs", 
                                        "20+Yrs"))
karamoja$District <- factor(karamoja$District)

karamoja_summary <- karamoja %>%
  dplyr::group_by(date, age_group, District) %>%
  dplyr::reframe(cases = sum(confirmed_cases, na.rm = TRUE)) %>%
  dplyr::arrange(District, date, age_group) 

karamoja_under_5 <- karamoja %>%
  dplyr::filter(age_group %in% c("0-28Dys", "29Dys-4Yrs")) %>%
  dplyr::group_by(date, District) %>%
  dplyr::reframe(cases = sum(confirmed_cases, na.rm = TRUE)) %>%
  dplyr::arrange(District, date)

ggplot(karamoja_under_5, aes(x = date, y = cases, col = District)) +
  geom_line() + theme_minimal() + facet_grid(District~., scales = "free_y")

# compare data seasonality and model seasonality 
seasonality <- function(data, months = 4, years = 2) {
  annual_cases <- sum(data$cases, na.rm = TRUE)/years
  prop <- numeric(0)
  for(i in 1:(years*12 - (months-1))) {
    prop[i] <- (sum(data$cases[i:(i+(months - 1))], na.rm = TRUE))/annual_cases
  }
  
  max <- max(prop)*100
  return(max)
}

karamoja_seasonality <- data.frame(district = unique(karamoja_under_5$District))

for(i in 1:nrow(karamoja_seasonality)) {
  df <- data.frame(dplyr::filter(karamoja_under_5,
                                 District == karamoja_seasonality$district[i]))
  karamoja_seasonality$four_month[i] <- seasonality(data = df,months = 4)
  karamoja_seasonality$five_month[i] <- seasonality(data = df,months = 5)
  karamoja_seasonality$six_month[i] <- seasonality(data = df,months = 6)
}

model_seasonality_func <- function(model_output, months = 4) {
  data <- model_output %>%
    dplyr::group_by(timestep) %>%
    dplyr::reframe(n_inc_clinical_1_1825 = median(n_inc_clinical_1_1825),
                   n_1_1825 = median(n_1_1825))
  
  years <- length(unique(data$timestep))/365
  len <- length(unique(data$timestep))
  annual_cases <- sum(data$n_inc_clinical_1_1825/data$n_1_1825,
                      na.rm = TRUE)/years
  days <- round(months * 30.25, digits = 0)
  
  prop <- numeric(0)
  for(i in 1:(len - (days-1))) {
    prop[i] <- (sum(data$n_inc_clinical_1_1825[i:(i+(days - 1))]/data$n_1_1825[i:(i+(days - 1))], 
                    na.rm = TRUE))/annual_cases
  }
  
  max <- max(prop)*100
  return(max)
  
}

model_seasonality <- data.frame(district = unique(model_out$district),
                                four_month = numeric(length(unique(model_out$district))),
                                five_month = numeric(length(unique(model_out$district))),
                                six_month = numeric(length(unique(model_out$district))))
for(i in 1:nrow(model_seasonality)) {
  df <- data.frame(dplyr::filter(model_out,
                                 district == model_seasonality$district[i]))
  model_seasonality$four_month[i] <- model_seasonality_func(model_output = df, months = 4)
  model_seasonality$five_month[i] <- model_seasonality_func(model_output = df, months = 5)
  model_seasonality$six_month[i] <- model_seasonality_func(model_output = df, months = 6)
}

karamoja_districts <- c("Kotido", "Moroto", "Nakapiripirit")

model <- model_seasonality %>%
  dplyr::filter(district %in% karamoja_districts) %>%
  tidyr::pivot_longer(four_month:six_month, names_to = "months", values_to = "model")
data <- karamoja_seasonality %>%
  dplyr::filter(district %in% karamoja_districts) %>%
  tidyr::pivot_longer(four_month:six_month, names_to = "months", values_to = "data")
compare <- full_join(model, data, by = c("district", "months"))
compare$months <- factor(compare$months, levels = c("four_month",
                                                    "five_month", 
                                                    "six_month"))

ggplot(data = compare) + theme_bw() +
  geom_rect(xmin = 60, xmax = Inf, ymin = 0, ymax = Inf, col = "grey85", fill = "grey85", alpha = 0.1) +
  geom_rect(xmin = 0, xmax = 60, ymin = 60, ymax = Inf, col = "grey85", fill = "grey85", alpha = 0.1) + 
  geom_point(data = compare, aes(x = model, y = data, col = months, shape = district)) + 
  geom_abline(slope = 1, intercept = 0, lty = 2) + xlim(c(40, 80)) + ylim(c(40, 80))

ggsave("seasonality_comparison.png", dpi = 500, width = 12, height = 7, units = "cm")  
