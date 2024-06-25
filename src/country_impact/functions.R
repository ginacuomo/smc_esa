model_seasonality_func <- function(model_output, months = 5) {
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
