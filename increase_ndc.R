#### Extra percentage

# annual co2 at year 2024
E0 = proj.evals.2024.ar1.const[["trajs.annual.quants"]]$CO22024[3]

# alpha: annual co2 at year 2024
c_2024 = proj.evals.2024.ar1.const[["trajs.annual.quants"]]$CO22024[3]

# annual temp at year 2024
x_2024 = real_data_adjusted$V6[nrow(real_data_adjusted)]

# Function to calculate beta_q for a given q
calculate_beta_q <- function(q) {
  beta_q <- qnorm(q, mean = 4.5e-4, sd = sqrt(3.24e-8))
  return(beta_q)
}

calculate_a <- function(X)
{
  c_func <-function(a)
  {
    return ((1-exp(-a * 76))/a - X/E0) # 76 = 2100-2024
  }
  library(pracma)
  root <- pracma::bisect(fun = c_func, 0.00001,0.3)$root
  return (root)
}

#######################################################

G_values <- c(2, 2, 2)
p_values <- c(0.5, 0.8, 0.95)

num_scenarios <- length(G_values)
ap = numeric(num_scenarios)


for (scenario in 1:num_scenarios) {
  G <- G_values[scenario]
  p <- p_values[scenario]
  
  beta_p <- calculate_beta_q(p)
  C_T <- (G - x_2024) / beta_p + c_2024
  ap[scenario] <- calculate_a(C_T)
} 

ac <- calculate_a(apply(proj.evals.2024.adjusted[["trajs.annual.quants.cont"]][3, 3:78], 1, cumsum)[76])
#ac <- calculate_a(apply(proj.evals.2024.adjusted[["trajs.annual.quants.cont"]][3, 2:78], 1, cumsum)[77])

(2-x_2024)/calculate_beta_q(0.5) + proj.evals.2024.adjusted[["trajs.annual.quants.cont"]][3,2]


extras <- matrix(nrow=7, ncol=num_scenarios)
data.medium = data_medium_wpp2024_updated2024
## for china
country = "CHN"
l <- which(paris.objective$country_code == country)
old_year <- paris.objective$old_year[l]
type <- paris.objective$Type[l]
size <- ifelse(type == 'Intensity', paris.objective$size[l], size <- paris.objective$ratio[l])
year_ind <- paris.objective$year[l] - 2023 # change 2014 here, forget to change last time
ref_level <- data.medium$Tech[(data.medium$Isocode == country) & 
                                (data.medium$Year == old_year)] * (1 - size / 100) # NDC goal of emission at year 2030 
if (ref_level <  data.medium$Tech[(data.medium$Isocode == country) & (data.medium$Year == 2024)]) {
  rate <- ref_level / data.medium$Tech[(data.medium$Isocode == country) & 
                                         (data.medium$Year == 2024)] 
  rate <- 1 - rate^(1/(paris.objective$year[l] - 2024)) # as
}else{
  rate <-  data.medium$Tech[(data.medium$Isocode == country) & (data.medium$Year == 2024)] / data.medium$Tech[(data.medium$Isocode == country) & 
                                                                                                                (data.medium$Year == old_year)] 
  rate <- 1 - rate^(1/(2024 -old_year)) # as
}
for (j in 1:num_scenarios) {
  # revised target emission at 2030
  obj <- data.medium$Tech[(data.medium$Isocode == country) & 
                            (data.medium$Year == 2024)] * (1 - rate * ap[j]/ac)^(paris.objective$year[l] - 2024) 
  
  extras[1,j] <- (1-obj/data.medium$Tech[(data.medium$Isocode == country) & 
                                           (data.medium$Year == old_year)])*100/size
}



# all the emission need to times 3.664 to be the actual emission in Mt
count <- 2
for (country in c('USA', 'DEU', 'JPN', 'RUS','GBR'))
{
  l <- which(paris.objective$country_code == country)
  old_year <- paris.objective$old_year[l]
  type <- paris.objective$Type[l]
  size <- ifelse(type == 'Emission', paris.objective$size[l], size <- paris.objective$ratio[l])
  year_ind <- paris.objective$year[l] - 2023 # change 2014 here, forget to change last time
  ref_level <- data.medium$CO2.total[(data.medium$Isocode == country) & 
                                       (data.medium$Year == old_year)] * (1 - size / 100) # NDC goal of emission at year 2030 
  if (ref_level <  data.medium$CO2.total[(data.medium$Isocode == country) & (data.medium$Year == 2024)]) {
    rate <- ref_level / data.medium$CO2.total[(data.medium$Isocode == country) & 
                                                (data.medium$Year == 2024)] 
    rate <- 1 - rate^(1/(paris.objective$year[l] - 2024)) # as
  }else{
    rate <-  data.medium$CO2.total[(data.medium$Isocode == country) & (data.medium$Year == 2024)] / data.medium$CO2.total[(data.medium$Isocode == country) & 
                                                (data.medium$Year == old_year)] 
    rate <- 1 - rate^(1/(2024 -old_year)) # as
    print(country)
  }
  
  if (rate > 0) {
  for (j in 1:num_scenarios) {
    # revised target emission at 2030
    obj <- data.medium$CO2.total[(data.medium$Isocode == country) & 
                                   (data.medium$Year == 2024)] * (1 - rate * ap[j]/ac)^(paris.objective$year[l] - 2024) 
    
    extras[count,j] <- (1-obj/data.medium$CO2.total[(data.medium$Isocode == country) & 
                                                      (data.medium$Year == old_year)])*100/size
  }
  } 
  
  count <- count + 1
  
}






## for india
country = "IND"
l <- which(paris.objective$country_code == country)
old_year <- paris.objective$old_year[l]
type <- paris.objective$Type[l]
size <- ifelse(type == 'Intensity', paris.objective$size[l], size <- paris.objective$ratio[l])

year_ind <- paris.objective$year[l] - 2023 # change 2014 here, forget to change last time

ref_level <- data.medium$Tech[(data.medium$Isocode == country) & 
                                (data.medium$Year == old_year)] * (1 - size / 100) # NDC goal of emission at year 2030 
if (ref_level <  data.medium$Tech[(data.medium$Isocode == country) & (data.medium$Year == 2024)]) {
  rate <- ref_level / data.medium$Tech[(data.medium$Isocode == country) & 
                                         (data.medium$Year == 2024)] 
  rate <- 1 - rate^(1/(paris.objective$year[l] - 2024)) # as
}else{
  rate <-  data.medium$Tech[(data.medium$Isocode == country) & (data.medium$Year == 2024)] / data.medium$Tech[(data.medium$Isocode == country) & 
                                                                                                                (data.medium$Year == old_year)] 
  rate <- 1 - rate^(1/(2024 -old_year)) # as
}
for (j in 1:num_scenarios) {
  # revised target emission at 2030
  obj <- data.medium$Tech[(data.medium$Isocode == country) & 
                            (data.medium$Year == 2024)] * (1 - rate * ap[j]/ac)^(paris.objective$year[l] - 2024) 
  
  extras[7,j] <- (1-obj/data.medium$Tech[(data.medium$Isocode == country) & 
                                           (data.medium$Year == old_year)])*100/size
}


numeric_columns <- sapply(extras, is.numeric)
extras[numeric_columns] <- (extras[numeric_columns] - 1) * 100
extras <- as.data.frame(extras)
extras$country <- c("CHN",'USA', 'DEU', 'JPN', 'RUS','GBR', 'IND')
library(xtable)

xtable(extras)
