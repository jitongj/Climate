library(gdata)
library(plyr)
library(countrycode)
library(xtable)
library(readxl)
library(reshape2)


#############
# CO2 Emission
setwd("~/Desktop/Supplementary/data_medium_updated_to_2024")
data.string.carbon1 <- paste0('National_Fossil_Carbon_Emissions_2024v1.xlsx') # with projection at year 2024
data.carbon1 <- read_excel(data.string.carbon1, skip = 11, sheet = 2)

data.carbon1 <- data.carbon1[-(1:109), ]
names(data.carbon1)[1] <- "Year"
names.country1 <- names(data.carbon1)[-1]


data.long.carbon1 <- melt(data.carbon1, id.vars = "Year", variable.name = "Country", value.name = "CO2.total")

data.long.carbon1$countrycode <- countrycode(data.long.carbon1$Country, origin = 'country.name', destination ='iso3c')
data.long.carbon1 <- data.long.carbon1[!is.na(data.long.carbon1$countrycode), ]

data.long.carbon1$Country <- countrycode(data.long.carbon1$countrycode, origin = 'iso3c', destination = 'country.name')
names(data.long.carbon1)[4] <- 'Isocode'

#############
# Population
data.string.pop1 <- paste0('WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx') # with projection at year 2023
data.pop1 <- read_excel(data.string.pop1, skip = 16, sheet = 1)
data.pop1 <- data.pop1[, c(3, 5, 11, 13)]
data.pop1_clean <- data.pop1[!is.na(data.pop1[, 3]), ]
selected_columns <- data.pop1_clean[, 1:2]
# Remove duplicate rows and get the combination of unique elements
unique_rows <- unique(selected_columns)
# Step 1: Prepare the pop_data data frame
years <- as.character(1950:2023)
col_names <- c("Country", "Country.code", years)
pop_data <- data.frame(matrix(NA, nrow = dim(unique_rows)[1], ncol = length(col_names)))
# Step 2: Fill in the first two columns of pop_data
pop_data[, 1] <- unique_rows[, 1]
pop_data[, 2] <- unique_rows[, 2]
names(pop_data) <- col_names
# pop_data is now ready and filled according to the specifications
# Step 3: Populate the rest of pop_data
for (i in 1:nrow(pop_data)) {
  for (year in years) { # Iterate through years as characters
    # Find indices where both country and country code match, and the year matches the column name
    j <- which(data.pop1[, 1] == as.character(unique_rows[i, 1]) & 
                 data.pop1[, 2] == as.character(unique_rows[i, 2]) & 
                 data.pop1[, 3] == year)
    
    # Check for exactly one match
    if (length(j) == 1) {
      # Assuming column names in pop_data match the years vector and data.pop1[, 4] contains the value to be copied
      pop_data[i, which(names(pop_data) == year)] <- data.pop1[j, 4]
    }
  }
}
# Ensure the columns are numeric before performing subtraction
# difference <- as.numeric(pop_data[["2021"]]) - as.numeric(pop_data[["2020"]])
# 
# # Proceed to calculate and add the 2022 and 2023 columns as before, ensuring numeric operations
# pop_data[["2022"]] <- as.numeric(pop_data[["2021"]]) + difference
# pop_data[["2023"]] <- difference + as.numeric(pop_data[["2022"]])
# 

# Ensure the columns are numeric before performing subtraction
difference <- as.numeric(pop_data[["2023"]]) - as.numeric(pop_data[["2022"]])

# Proceed to calculate and add the 2023 and 2024 columns as before, ensuring numeric operations
pop_data[["2024"]] <- as.numeric(pop_data[["2023"]]) + difference


pop_data <- melt(pop_data, id = c('Country', 'Country.code'))
names(pop_data) <- c('Country', 'countrycode', 'Year', 'Pop')
pop_data$Isocode <- countrycode(pop_data$Country, origin = 'country.name', destination = 'iso3c')
pop_data <- pop_data[!is.na(pop_data$Isocode), ]
pop_data$Country <- countrycode(pop_data$Isocode, origin = 'iso3c', destination = 'country.name')
pop_data <- pop_data[pop_data$countrycode != 948, ] # 删除Less developed regions, excluding China
pop_data <- pop_data[pop_data$countrycode != 2093, ] # 删除SIDS Atlantic, Indian Ocean and South China Sea (AIS)
## not run
#######################
# # Find elements that are in the first column of pop_data but not in the first column of data.pop
# unique_in_pop_data <- setdiff(pop_data[[1]], data.pop[[1]])
# # Find elements that are in the first column of data.pop but not in the first column of pop_data
# unique_in_data_pop <- setdiff(data.pop[[1]], pop_data[[1]])
# print(unique_in_pop_data)#Guernsey, Jersey, St. Barthélemy, Saint Martin (French part)
# print(unique_in_data_pop)#empty
# # Find the rows in the first column of pop_data that exist in unique_in_pop_data
# rows_to_remove <- pop_data[[1]] %in% unique_in_pop_data
# # delete those rows
# pop_data <- pop_data[!rows_to_remove, ]
########################


#########
# GDP
data.string.gdp1 <- paste0("gdp_madd2024_imf.xlsx")
data.gdp.new1 <- read_excel(data.string.gdp1)
# Remove some countries
#removed_countries <- c();
#data.gdp.new1 <- data.gdp.new[!(data.gdp.new1$Countrycode == removed_countries),]

data.gdp.new1$Countrycode <- as.character(data.gdp.new1$Countrycode)
data.gdp.new1$Country <- as.character(data.gdp.new1$Country)
data.gdp.new1[, 3:ncol(data.gdp.new1)] <- lapply(data.gdp.new1[, 3:ncol(data.gdp.new1)], as.numeric)

data.gdp.new1 <- melt(data.gdp.new1, id = c('Countrycode', 'Country'))
data.gdp.new1 <- data.gdp.new1[order(data.gdp.new1$Countrycode, data.gdp.new1$Country), ]
names(data.gdp.new1) <- c('Isocode', 'Country', 'Year', 'GDP')
#data.gdp.long.new <- data.gdp.new[, c('countrycode', 'country', 'year', 'rgdpnapc')]
#names(data.gdp.long.new) <- c('Isocode', 'Country', 'Year', 'GDP')



#####
#data_medium1 <- merge(pop_data, data.long.carbon1, by = c('Isocode', 'Country', 'Year'))
#data_medium1 <- merge(data_medium1, data.gdp.new1, by = c('Isocode', 'Country', 'Year'))
#
data_medium1 <- merge(pop_data, data.long.carbon1, by = c('Isocode', 'Year'))
data_medium1 <- merge(data_medium1, data.gdp.new1, by = c('Isocode', 'Year'))

data_medium1$CO2.total <- as.numeric(data_medium1$CO2.total)
data_medium1$CO2.total[is.na(data_medium1$CO2.total)] <- NaN # 替换NA为NaN

data_medium1$Pop <- as.numeric(data_medium1$Pop)

data_medium1$CO2 <- data_medium1$CO2.total / data_medium1$Pop # CO2 per pop
data_medium1$Isocode <- factor(data_medium1$Isocode)
data_medium1$CO2 <- data_medium1$CO2 * (11/3)
data_medium1$CO2 <- data_medium1$CO2 * 1000

data_medium1$Tech <- data_medium1$CO2/data_medium1$GDP * 10^4 

# 
num_rows <- nrow(data_medium1)

# 创建一个具有相同行数和9个变量的data_medium2
data_medium2 <- data.frame(Isocode = rep(NA, num_rows),
                           Country = rep(NA, num_rows),
                           Year = rep(NA, num_rows),
                           countrycode = rep(NA, num_rows),
                           Pop = rep(NA, num_rows),
                           CO2.total = rep(NA, num_rows),
                           GDP = rep(NA, num_rows),
                           CO2 = rep(NA, num_rows),
                           Tech = rep(NA, num_rows))


data_medium2$Isocode<-data_medium1$Isocode
data_medium2$Country<-data_medium1$Country
data_medium2$Year<-data_medium1$Year
data_medium2$countrycode<-data_medium1$countrycode
data_medium2$Pop<-data_medium1$Pop
data_medium2$CO2.total <- data_medium1$CO2.total
data_medium2$GDP <-data_medium1$GDP
data_medium2$CO2 <- data_medium1$CO2
data_medium2$Tech <-data_medium1$Tech

# 转化数据类型
data_medium2$Year <- as.numeric(as.character(data_medium2$Year))
data_medium2$countrycode <- as.integer(data_medium2$countrycode)
data_medium2$GDP <- as.integer(data_medium2$GDP)

##
# 指定要检查的元素
elements_to_remove <- c("AFG", "LBN", "LKA", "PRK", "CUB", "SYR", "GNB") # "PSU", "SUN"

# 找出Isocode列中包含这些元素的行
rows_to_remove <- data_medium2$Isocode %in% elements_to_remove

# 选择不包含这些元素的行，更新data_medium2
data_medium_wpp2024_updated2024 <- data_medium2[!rows_to_remove, ]

##
library(openxlsx)
write.xlsx(data_medium_wpp2024_updated2024, file = "data_medium_wpp2024_updated2024.xlsx")
# Save the data_medium_new data frame as an R data file
save(data_medium_wpp2024_updated2024, file = "data_medium_wpp2024_updated2024.rda")


