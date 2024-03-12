# comparing the uganda routine data vs the model output

# orderly set up first off
orderly2::orderly_resource("data/uganda-jan-21-feb-22.csv")
orderly2::orderly_resource("data/uganda-march-22-dec-22.csv")
orderly2::orderly_resource("data/WPP2022.csv")
orderly2::orderly_resource("karamoja.RDS")
orderly2::orderly_resource("shape_file.RDS")
orderly2::orderly_parameters(district = "NA", calibrated = NULL, repetitions = NULL)
# orderly2::orderly_artefact("output/seasonality_comparison.png")
# orderly2::orderly_artefact("output/nmf_adjustment.pdf")

library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)

# start with the model outputs
districts <- readRDS("karamoja.RDS")
output<- data.table()
for(i in 1:length(districts)) {
  district <- districts[i]
  metadata <- orderly2::orderly_dependency("run_counterfactual",
                                           "latest(parameter:district == this:district &&
                                                        parameter:calibrated == this:calibrated &&
                                                        parameter:repetitions == this:repetitions)",
                                           c(df.RDS = "df.RDS"))
  dt <- readRDS(metadata$files$here)
  output <- rbind(output, dt, fill = T)
}

# aggregate data
out_agg <- output %>%
  dplyr::mutate(date = as.Date(timestep, origin = "2021-01-01"),
                month = format(as.Date(date), "%Y-%m")) %>%
  dplyr::group_by(district, month, repetition) %>%
  dplyr::reframe(cases = sum(n_inc_clinical_1_1825))

if(length(unique(out_agg$district))!=9) {
  stop("missing districts")
}

quantile_95 <- function(x) {
  quantile(x, probs = c(0.025, 0.5, 0.975))
}

out_agg_q95 <- out_agg %>%
  dplyr::group_by(district, month) %>%
  dplyr::reframe(q2.5 = quantile_95(cases)[1],
                 median = quantile_95(cases)[2],
                 q97.5 = quantile_95(cases)[3]) %>%
  dplyr::filter(month < format(as.Date("2023-02-01"),"%Y-%m")) %>%
  dplyr::mutate(month = as.Date(paste(month, "-01", sep="")))

ggplot(out_agg_q95, aes(x = month, y = median, group = district)) + facet_wrap(.~district, scales = "free_y") + 
  geom_line() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# read in routine data
uga1 <- read.csv("data/uganda-jan-21-feb-22.csv", header = FALSE)
uga2 <- read.csv("data/uganda-march-22-dec-22.csv", header = FALSE)
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
  dplyr::mutate(age_group = "Under 5 years") %>%
  dplyr::arrange(District, date)

karamoja_over_5 <- karamoja %>%
  dplyr::filter(age_group == "5-9Yrs") %>%
  dplyr::group_by(date, District) %>%
  dplyr::reframe(cases = sum(confirmed_cases, na.rm = TRUE)) %>% 
  dplyr::mutate(age_group = "5 - 9 years") %>%
  dplyr::arrange(District, date)

karamoja_over_10 <- karamoja %>%
  dplyr::filter(age_group == "10-19Yrs") %>%
  dplyr::group_by(date, District) %>%
  dplyr::reframe(cases = sum(confirmed_cases, na.rm = TRUE)) %>% 
  dplyr::mutate(age_group = "10 - 19 years") %>%
  dplyr::arrange(District, date)

karamoja_age <- rbind(karamoja_under_5, karamoja_over_5, karamoja_over_10)
karamoja_age$district <- karamoja_age$District

ggplot() + geom_line(data = karamoja_age, aes(x = date, y = cases, 
                                              group = age_group, col = age_group)) +
  facet_wrap(.~district, scales = "free_y") + 
  geom_line() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line(data = out_agg_q95, aes(x = month, y = median), lty = 2) # make visual check easier

#######################################################################################################

plot_function <- function(data, model, district, year1 = NA, year2 = NA) {
  df <- data[data$district == district,]
  model_df <- model[model$district == district,]
  
  if(year1 == TRUE) {
    smc_times1 <- c(as.Date("2021-05-01"), 
                   as.Date("2021-06-01"),
                   as.Date("2021-07-01"),
                   as.Date("2021-08-01"),
                   as.Date("2021-09-01"))
  } else {
    smc_times1 <- as.Date(x = integer(0), origin = "1970-01-01")
  } 
  if(year2 == TRUE) {
    smc_times2 <- c(as.Date("2022-06-01"),
                    as.Date("2022-07-01"),
                    as.Date("2022-08-01"),
                    as.Date("2022-09-01"),
                    as.Date("2022-10-01"))
  } else {
    smc_times2 <- as.Date(x = integer(0), origin = "1970-01-01")
  }
  
  smc_times <- c(smc_times1, smc_times2)
  
  ggplot() + geom_line(data = df, 
                       aes(x = date, y = cases, 
                                      group = age_group, col = age_group)) +
    
    geom_line() +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = smc_times, lty = 2, lwd = 0.5) + 
    labs(title = paste(district)) +
    geom_line(data = model_df, aes(x = month, y = median))
  
}

dist_vec <- unique(karamoja_age$District)
dist_df <- data.frame(district = dist_vec,
                      year1 = c(FALSE,
                                FALSE,
                                FALSE,
                                FALSE,
                                TRUE,
                                TRUE,
                                FALSE,
                                FALSE,
                                FALSE),
                      year2 = c(FALSE,
                                TRUE,
                                FALSE,
                                FALSE,
                                TRUE,
                                TRUE,
                                TRUE,
                                TRUE,
                                FALSE))

pdf(width = 12, height = 8, "output/district_seasonality.pdf")
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[1],year1 = dist_df$year1[1],year2 = dist_df$year2[1])
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[2],year1 = dist_df$year1[2],year2 = dist_df$year2[2])
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[3],year1 = dist_df$year1[3],year2 = dist_df$year2[3])
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[4],year1 = dist_df$year1[4],year2 = dist_df$year2[4])
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[5],year1 = dist_df$year1[5],year2 = dist_df$year2[5])
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[6],year1 = dist_df$year1[6],year2 = dist_df$year2[6])
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[7],year1 = dist_df$year1[7],year2 = dist_df$year2[7])
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[8],year1 = dist_df$year1[8],year2 = dist_df$year2[8])
plot_function(data = karamoja_age, model = out_agg_q95,
              district = dist_df$district[9],year1 = dist_df$year1[9],year2 = dist_df$year2[9])
dev.off()

ggplot(karamoja_age, aes(x = date, y = cases, col = age_group, group = age_group)) +
  geom_line() + theme_minimal() + facet_grid(District~., scales = "free_y") + 
  expand_limits(y = 0)

karamoja_ratio <- karamoja_summary %>% 
  tidyr::pivot_wider(names_from = age_group, values_from = cases) %>%
  dplyr::select(date, District, `29Dys-4Yrs`, `10-19Yrs`) %>%
  dplyr::group_by(date, District) %>%
  dplyr::reframe(ratio = `29Dys-4Yrs`/`10-19Yrs`)

plot_ratio_function <- function(data, district, year1 = NA, year2 = NA) {
  df <- data %>% dplyr::filter(District == district)
  
  if(year1 == TRUE) {
    smc_times1 <- c(as.Date("2021-05-01"), 
                    as.Date("2021-06-01"),
                    as.Date("2021-07-01"),
                    as.Date("2021-08-01"),
                    as.Date("2021-09-01"))
  } else {
    smc_times1 <- as.Date(x = integer(0), origin = "1970-01-01")
  } 
  if(year2 == TRUE) {
    smc_times2 <- c(as.Date("2022-06-01"),
                    as.Date("2022-07-01"),
                    as.Date("2022-08-01"),
                    as.Date("2022-09-01"),
                    as.Date("2022-10-01"))
  } else {
    smc_times2 <- as.Date(x = integer(0), origin = "1970-01-01")
  }
  
  smc_times <- c(smc_times1, smc_times2)
  
  ggplot(df, aes(x = date, y = ratio, col = District)) +
    geom_line() + theme_minimal() +
    geom_vline(xintercept = smc_times, lty = 2, lwd = 0.8) + 
    labs(title = district) + expand_limits(y = 0) +
    geom_hline(yintercept = 1)
  
}

plot_ratio_function(data = karamoja_ratio, district = "Kotido", year1 = TRUE, year2 = TRUE)

pdf(width = 12, height = 8, "output/district_ratio.pdf")
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[1],year1 = dist_df$year1[1],year2 = dist_df$year2[1])
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[2],year1 = dist_df$year1[2],year2 = dist_df$year2[2])
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[3],year1 = dist_df$year1[3],year2 = dist_df$year2[3])
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[4],year1 = dist_df$year1[4],year2 = dist_df$year2[4])
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[5],year1 = dist_df$year1[5],year2 = dist_df$year2[5])
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[6],year1 = dist_df$year1[6],year2 = dist_df$year2[6])
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[7],year1 = dist_df$year1[7],year2 = dist_df$year2[7])
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[8],year1 = dist_df$year1[8],year2 = dist_df$year2[8])
plot_ratio_function(data = karamoja_ratio, district = dist_df$district[9],year1 = dist_df$year1[9],year2 = dist_df$year2[9])
dev.off()


####################################################################################
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

model_seasonality_func <- function(model_output, district, months = 4) {
  data <- model_output %>%
    dplyr::filter(district == district) %>%
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

model_seasonality <- data.frame(district = unique(output$district),
                                four_month = numeric(length(unique(output$district))),
                                five_month = numeric(length(unique(output$district))),
                                six_month = numeric(length(unique(output$district))))
for(i in 1:nrow(model_seasonality)) {
  df <- data.frame(dplyr::filter(output,
                                 district == model_seasonality$district[i]))
  model_seasonality$four_month[i] <- model_seasonality_func(model_output = df, months = 4)
  model_seasonality$five_month[i] <- model_seasonality_func(model_output = df, months = 5)
  model_seasonality$six_month[i] <- model_seasonality_func(model_output = df, months = 6)
}

model <- model_seasonality %>%
  dplyr::filter(district %in% districts) %>%
  tidyr::pivot_longer(four_month:six_month, names_to = "months", values_to = "model")
data <- karamoja_seasonality %>%
  dplyr::filter(district %in% districts) %>%
  tidyr::pivot_longer(four_month:six_month, names_to = "months", values_to = "data")
compare <- full_join(model, data, by = c("district", "months"))
compare$months <- factor(compare$months, levels = c("four_month",
                                                    "five_month", 
                                                    "six_month"))

ggplot(data = compare) + theme_bw() +
  geom_point(data = compare, aes(x = model, y = data)) + facet_grid(months ~ .) +
  geom_abline(slope = 1, intercept = 0, lty = 2) + xlim(c(40, 80)) +
  ylim(c(40, 80)) +
  xlim(c(40, 80))

ggsave("output/seasonality_comparison.png", dpi = 500, width = 12, height = 7, units = "cm") 

## adjusting for non-malarial fevers
mean_nmf_frequency = c(148.578, 139.578, 141.564, 155.874, 179.364,
                       216.192, 233.478, 268.056, 312.858, 315.564,
                       285.156, 255.246, 238.302, 216.618)
mean_nmf_rate <- 1/mean_nmf_frequency
nmf_age_brackets = c(-0.1, 365.0, 730.0, 1095.0, 1460.0, 1825.0,
                     2555.0, 3285.0, 4015.0, 4745.0, 5475.0,
                     7300.0, 9125.0, 10950.0, 36850.0)

# average out the NMF rate across the whole of 0 - 5 years
# uganda demography
demog <- read_csv("data/WPP2022.csv", skip = 16)
names(demog) <- snakecase::to_snake_case(names(demog))

# filter for Madagascar and most recent estimate
demog <- demog %>% dplyr::filter(iso_3_alpha_code == "UGA") %>%
  dplyr::filter(year == 2021)

# these columns were a different class to the others
demog$"95" <- as.character(demog$"95") 
demog$"96" <- as.character(demog$"96") 
demog$"97" <- as.character(demog$"97") 
demog$"98" <- as.character(demog$"98") 
demog$"99" <- as.character(demog$"99") 
demog$"100" <- as.character(demog$"100") 

# make df long not wide
demog_long <- demog %>% 
  tidyr::pivot_longer("0":"100", names_to = "age", values_to = "pop") %>%
  dplyr::mutate(pop = gsub(" ","", pop)) %>%
  dplyr::mutate(pop = as.numeric(pop)) %>% # data is in thousands 
  dplyr::mutate(age = as.numeric(age))

ages <- c("0_1_yrs", "1_2_yrs", "2_3_yrs", "3_4_yrs", "4_5_yrs")
# estimated population size in each age class
pop1 <- demog_long %>%
  dplyr::filter(age %in% 0) %>% 
  dplyr::reframe(population = sum(pop)) %>%
  dplyr::pull(population)
pop2 <- demog_long %>%
  dplyr::filter(age %in% 1) %>% 
  dplyr::reframe(population = sum(pop)) %>%
  dplyr::pull(population)
pop3 <- demog_long %>%
  dplyr::filter(age %in% 2) %>% 
  dplyr::reframe(population = sum(pop)) %>%
  dplyr::pull(population)
pop4 <- demog_long %>%
  dplyr::filter(age %in% 3) %>% 
  dplyr::reframe(population = sum(pop)) %>%
  dplyr::pull(population)
pop5 <- demog_long %>%
  dplyr::filter(age %in% 4) %>% 
  dplyr::reframe(population = sum(pop)) %>%
  dplyr::pull(population)
total_pop <- pop1 + pop2 + pop3 + pop4 + pop5

# proportion of population in each age group
proportion <- c(pop1/total_pop,
                pop2/total_pop,
                pop3/total_pop,
                pop4/total_pop,
                pop5/total_pop)

frequency_fever <- proportion[1] * mean_nmf_frequency[1] +
  proportion[2] * mean_nmf_frequency[2] +
  proportion[3] * mean_nmf_frequency[3] +
  proportion[4] * mean_nmf_frequency[4] +
  proportion[5] * mean_nmf_frequency[5]
rate_fever = 1/frequency_fever

output$nmf <- output$n_detect_1_1825 * rate_fever
output$nmf_adjusted_cases <- output$nmf + output$n_inc_clinical_1_1825

out_agg_nmf <- output %>%
  dplyr::mutate(date = as.Date(timestep, origin = "2021-01-01"),
                month = format(as.Date(date), "%Y-%m")) %>%
  dplyr::group_by(district, month, repetition) %>%
  dplyr::reframe(cases = sum(nmf_adjusted_cases))

out_agg_q95_nmf <- out_agg_nmf %>%
  dplyr::group_by(district, month) %>%
  dplyr::reframe(q2.5 = quantile_95(cases)[1],
                 median = quantile_95(cases)[2],
                 q97.5 = quantile_95(cases)[3]) %>%
  dplyr::filter(month < format(as.Date("2023-02-01"),"%Y-%m")) %>%
  dplyr::mutate(month = as.Date(paste(month, "-01", sep="")))


plot_function_nmf <- function(data, model, model_nmf, 
                              district, year1 = NA, year2 = NA) {
  df <- data[data$district == district,]
  model_nmf <- model_nmf[model_nmf$district == district,]
  model_df <- model[model$district == district,]
  
  model_nmf$model <- "Non-malarial fever adjustment"
  model_df$model <- "Standard model output"
  model_plot <- rbind(model_nmf, model_df)
  
  if(year1 == TRUE) {
    smc_times1 <- c(as.Date("2021-05-01"), 
                    as.Date("2021-06-01"),
                    as.Date("2021-07-01"),
                    as.Date("2021-08-01"),
                    as.Date("2021-09-01"))
  } else {
    smc_times1 <- as.Date(x = integer(0), origin = "1970-01-01")
  } 
  if(year2 == TRUE) {
    smc_times2 <- c(as.Date("2022-06-01"),
                    as.Date("2022-07-01"),
                    as.Date("2022-08-01"),
                    as.Date("2022-09-01"),
                    as.Date("2022-10-01"))
  } else {
    smc_times2 <- as.Date(x = integer(0), origin = "1970-01-01")
  }
  
  smc_times <- c(smc_times1, smc_times2)
  
  ggplot() + geom_line(data = df, 
                       aes(x = date, y = cases, 
                           group = age_group, col = age_group)) +
    
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_vline(xintercept = smc_times, lty = 2, lwd = 0.5) + 
    labs(title = paste(district)) +
    geom_line(data = model_plot, aes(x = month, y = median, lty = model))
  
}

pdf(width = 12, height = 8, "output/district_seasonality_nmf.pdf")
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[1],year1 = dist_df$year1[1],year2 = dist_df$year2[1])
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[2],year1 = dist_df$year1[2],year2 = dist_df$year2[2])
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[3],year1 = dist_df$year1[3],year2 = dist_df$year2[3])
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[4],year1 = dist_df$year1[4],year2 = dist_df$year2[4])
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[5],year1 = dist_df$year1[5],year2 = dist_df$year2[5])
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[6],year1 = dist_df$year1[6],year2 = dist_df$year2[6])
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[7],year1 = dist_df$year1[7],year2 = dist_df$year2[7])
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[8],year1 = dist_df$year1[8],year2 = dist_df$year2[8])
plot_function_nmf(data = karamoja_age, model = out_agg_q95, model_nmf = out_agg_q95_nmf, 
                  district = dist_df$district[9],year1 = dist_df$year1[9],year2 = dist_df$year2[9])
dev.off()

ggplot(dplyr::filter(output, repetition == 1), 
       aes(x = timestep, y = n_detect_1_1825/n_1_1825)) + geom_line() +
  facet_grid(district ~ .) + theme_bw() + expand_limits(y = 0)

output %>% dplyr::select(n_detect_1_1825, n_1_1825, nmf,
                         repetition, timestep, district) %>%
  dplyr::filter(repetition == 1) %>% 
  dplyr::filter(timestep < 366) %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(sum(nmf)/median(n_1_1825))

model_seasonality_nmf_func <- function(model_output_nmf, district, months = 4) {
  data <- model_output_nmf %>%
    dplyr::filter(district == district) %>%
    dplyr::group_by(timestep) %>%
    dplyr::reframe(nmf_adjusted_cases = median(nmf_adjusted_cases),
                   n_1_1825 = median(n_1_1825))
  
  years <- length(unique(data$timestep))/365
  len <- length(unique(data$timestep))
  annual_cases <- sum(data$nmf_adjusted_cases/data$n_1_1825,
                      na.rm = TRUE)/years
  days <- round(months * 30.25, digits = 0)
  
  prop <- numeric(0)
  for(i in 1:(len - (days-1))) {
    prop[i] <- (sum(data$nmf_adjusted_cases[i:(i+(days - 1))]/data$n_1_1825[i:(i+(days - 1))], 
                    na.rm = TRUE))/annual_cases
  }
  
  max <- max(prop)*100
  return(max)
  
}

model_seasonality_nmf <- data.frame(district = unique(output$district),
                                four_month = numeric(length(unique(output$district))),
                                five_month = numeric(length(unique(output$district))),
                                six_month = numeric(length(unique(output$district))))
for(i in 1:nrow(model_seasonality_nmf)) {
  df <- data.frame(dplyr::filter(output,
                                 district == model_seasonality_nmf$district[i]))
  model_seasonality_nmf$four_month[i] <- model_seasonality_nmf_func(model_output_nmf = df, months = 4)
  model_seasonality_nmf$five_month[i] <- model_seasonality_nmf_func(model_output_nmf = df, months = 5)
  model_seasonality_nmf$six_month[i] <- model_seasonality_nmf_func(model_output_nmf = df, months = 6)
}

model_nmf <- model_seasonality_nmf %>%
  dplyr::filter(district %in% districts) %>%
  tidyr::pivot_longer(four_month:six_month, names_to = "months", values_to = "model")

model <- model_seasonality %>%
  dplyr::filter(district %in% districts) %>%
  tidyr::pivot_longer(four_month:six_month, names_to = "months", values_to = "model")

data_seasonality <- karamoja_seasonality %>%
  tidyr::pivot_longer(four_month:six_month, names_to = "months", values_to = "data")

seasonal_compare <- dplyr::left_join(model, data_seasonality) %>%
  dplyr::mutate(nmf = "no NMF adjustment")
seasonal_compare_nmf <- dplyr::left_join(model_nmf, data_seasonality) %>%
  dplyr::mutate(nmf = "NMF adjustment")

compare_nmf <- rbind(seasonal_compare, seasonal_compare_nmf) 

ggplot(compare_nmf, aes(x = data, y = model, col = district)) + geom_point() + theme_bw() +
  facet_grid(months ~ nmf) + geom_abline(slope = 1, intercept = 0, lty = 2)
ggsave("output/nmf_adjustment.pdf", units = "cm", width = 20, height = 20, dpi = 300)
ggplot(compare_nmf, aes(x = data, y = model, col = district)) + geom_point() + theme_bw() +
  facet_grid(nmf ~ months) + geom_abline(slope = 1, intercept = 0, lty = 2) + 
  theme(legend.position = "bottom")
ggsave("output/nmf_adjustment.png", units = "cm", width = 20, height = 15, dpi = 300)

out_agg_q95$adjustment <- "no NMF"
out_agg_q95_nmf$adjustment <- "NMF"
out_agg <- rbind(out_agg_q95, out_agg_q95_nmf)

ggplot() + geom_line(data = filter(karamoja_age, age_group == "Under 5 years"), 
                     aes(x = date, y = cases, group = age_group, col = age_group)) +
  facet_wrap(.~district, scales = "free_y") + theme_bw() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line(data = out_agg, aes(x = month, y = median, lty = adjustment)) +
  theme(legend.position = "bottom")
ggsave("output/nmf_vs_data.png", dpi = 500, width = 20, height = 12, units = "cm")

## now look at the prevalence over time - see if this is the cause of the relationship we see
out_compare <- output %>%
  dplyr::mutate(date = as.Date(timestep, origin = "2021-01-01"),
                month = format(as.Date(date), "%Y-%m")) %>%
  dplyr::group_by(district, month, repetition) %>%
  dplyr::reframe(cases = sum(n_inc_clinical_1_1825),
                 prev = mean(n_detect_1_1825)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(district, month) %>%
  dplyr::reframe(median_cases = median(cases),
                 median_prev = median(prev))
out_compare_long <- out_compare %>%
  pivot_longer(cols = median_cases:median_prev, names_to = "parameter", values_to = "model_estimate")



ggplot(out_compare_long) + geom_line(aes(x = month, y = model_estimate, 
                                         group = parameter, col = parameter)) +
  facet_wrap(.~district, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw()
ggsave("output/prevalence_vs_cases.png", dpi = 500, width = 12, height = 7, units = "cm")


out_prev <- output %>%
  dplyr::group_by(district, timestep, repetition) %>%
  dplyr::reframe(prevalence = n_detect_1_1825/n_1_1825) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(district, timestep) %>%
  dplyr::reframe(prevalence = median(prevalence))
ggplot(out_prev) + geom_line(aes(x = timestep, y = prevalence)) + facet_wrap(.~district, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw() + expand_limits(y = 0)

output_nmf_long <- output %>%
  dplyr::select(timestep, district, repetition, nmf, nmf_adjusted_cases, n_inc_clinical_1_1825) %>%
  pivot_longer(nmf:n_inc_clinical_1_1825, names_to = "variable", values_to = "model_estimate") %>%
  dplyr::group_by(timestep, district, variable) %>%
  dplyr::reframe(model_estimate = median(model_estimate))

ggplot(output_nmf_long) + geom_line(aes(x = timestep, y = model_estimate, col = variable, group = variable)) + 
  facet_wrap(.~district, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw() + expand_limits(y = 0)
