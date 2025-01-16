setwd("~/Documents/UW Courses/Research/CO2_Data_wpp2024_updated_to_2024")
get_temp_prob <- function(proj_list) {
  proj_temp <- data.frame(proj_list$projection)
  sample = dim(proj_temp)[2]-1
  
  # For year 2100
  temp_2100 <- proj_temp[76, -1] - mean(real_data_adjusted$V6[12:31])# 76 means year 2100
  under2C_2100 <- sum(temp_2100 < 2, na.rm = TRUE) / sample
  under1.5C_2100 <- sum(temp_2100 < 1.5, na.rm = TRUE) / sample
  above3C_2100 <- sum(temp_2100 > 3, na.rm = TRUE) / sample
  
  # For years 2081-2100
  avg_temp_2081_2100 <- apply(proj_temp[57:76, -1], 2, mean, na.rm = TRUE) - mean(real_data_adjusted$V6[12:31])# 57:76 means year 2081:2100
  under2C_2081_2100 <- sum(avg_temp_2081_2100 < 2, na.rm = TRUE) / sample
  under1.5C_2081_2100 <- sum(avg_temp_2081_2100 < 1.5, na.rm = TRUE) / sample
  above3C_2081_2100 <- sum(avg_temp_2081_2100 > 3, na.rm = TRUE) / sample
  
  # Combine into a data frame
  result <- data.frame(
    Metric = c("Under 2C (2100)", "Under 1.5C (2100)", "Above 3C (2100)", "Under 2C (2081-2100)", "Under 1.5C (2081-2100)", "Above 3C (2081-2100)"),
    Probability = c(under2C_2100, under1.5C_2100, above3C_2100,
                    under2C_2081_2100, under1.5C_2081_2100, above3C_2081_2100)
  )
  return(result)
}

prob_none = get_temp_prob(proj_list)
prob_adj = get_temp_prob(proj_list_adjusted)
prob_con = get_temp_prob(proj_list_adjusted_cont)
prob_usa = get_temp_prob(proj_list_adjusted_usa)

# Create the data frame
conbined_prob <- data.frame(
  none = prob_none$Probability,
  ajd = prob_adj$Probability,
  con = prob_con$Probability,
  usa = prob_usa$Probability
)

# Set custom row names
rownames(conbined_prob) <- c(
  "Under 2C (2100)", 
  "Under 1.5C (2100)", 
  "Above 3C (2100)",
  "Under 2C (2081-2100)", 
  "Under 1.5C (2081-2100)",
  "Above 3C (2081-2100)"
)
conbined_prob_t = t(conbined_prob)
conbined_prob_t = as.data.frame(conbined_prob_t)
library(writexl)
write_xlsx(conbined_prob_t, "new_prob_output.xlsx")




