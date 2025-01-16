## HadCRUT4
real_data <- read.table('had4_krig_annual_v2_0_0.txt')
real_data$V2 <- real_data$V2 + 287.75 - real_data$V2[156] # 287.75: global mean temp in kelvin
real_data$V6 <- real_data$V2 - 273.15 #kelvin to celsius
save(real_data, file = 'real_data.Rda')

## HadCRUT5
setwd("~/Desktop/Supplementary/HadCRUT5")
library(readxl)
hadcrut5 <- read_excel("hadcrut5_annual_ver5.0.2.0.xlsx", col_names = FALSE)
colnames(hadcrut5) <- c("V1", "V2", "V3", "V4", "V5")
hadcrut5$V2 <- hadcrut5$V2 + 287.75 - hadcrut5$V2[156] # 287.75: global mean temp in kelvin
hadcrut5$V6 <- hadcrut5$V2 - 273.15 #kelvin to celsius
hadcrut5 <- as.data.frame(hadcrut5)
save(hadcrut5, file = 'real_hadcrut5.Rda')

## HadCRUT5 updated to 2024
setwd("~/Desktop/Supplementary/HadCRUT5")
library(readxl)
hadcrut5 <- read_excel("hadcrut5_annual_ver5.0.2.0_updated_to_2024.xlsx", col_names = FALSE)
colnames(hadcrut5) <- c("V1", "V2", "V3", "V4", "V5")
hadcrut5$V2 <- hadcrut5$V2 + 287.75 - hadcrut5$V2[156] # 287.75: global mean temp in kelvin
hadcrut5$V6 <- hadcrut5$V2 - 273.15 #kelvin to celsius
hadcrut5 <- as.data.frame(hadcrut5)
save(hadcrut5, file = 'real_hadcrut5_updated_to_2024.Rda')