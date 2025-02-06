library(ggplot2)
library(gridExtra)
library(scales)
library(doBy)
library(cowplot)
library(dplyr)
#################################################
#################### figure1 ####################
#################################################

###############################
######### emission ############
###############################
plot.co2.projections.world <- function(data.medium.pre,
                                       data.medium.post,
                                 trajs.quants,
                                 plot.rcp=FALSE,
                                 ybreaks=seq(0,120,by=20),
                                 trajs.quants.2015=NULL,
                                 title="World",
                                 ylabel="Yearly CO2 Emission",
                                 xlabel = "(a)",
                                 additional_line=NULL) {

  data.restriction.pre <- data.medium.pre[!is.na(data.medium.pre$CO2),]
  data.restriction.post <- data.medium.post[!is.na(data.medium.post$CO2),]
  
  co2.sums.na.rm.pre <- summaryBy(CO2.total~ Year, data.restriction.pre, FUN=function(x) sum(x, na.rm=T))
  co2.sums.na.rm.pre <- co2.sums.na.rm.pre[co2.sums.na.rm.pre$Year >= 2005 & co2.sums.na.rm.pre$Year <= 2015, ]
  co2.sums.na.rm.post <- summaryBy(CO2.total~ Year, data.restriction.post, FUN=function(x) sum(x, na.rm=T))
  co2.sums.na.rm.post <- co2.sums.na.rm.post[co2.sums.na.rm.post$Year >= 2016, ]
  co2.sums.na.rm <- rbind(co2.sums.na.rm.pre, co2.sums.na.rm.post)
  
  names(co2.sums.na.rm)[2] <- "Carbon"
  co2.sums.na.rm$Carbon <- co2.sums.na.rm$Carbon / 1e3 * 11/3
  
  year.start <- 2010
  year.end <- 2024
  x.vals <- seq(year.start, year.end, by=5)
  
  # Create plot
  plot.obj <- ggplot() +
    labs(color="Scenario") +
    theme(panel.background=element_rect(fill="white"),
          panel.grid.minor=element_line(color="white"),
          panel.grid.major=element_line(color="grey90"),
          text=element_text(family="Arial", size=16),
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 12)) +  # Center the title
    scale_y_continuous(breaks=pretty_breaks(n=4)) +
    scale_x_continuous(breaks=seq(2005, 2024, by=5), limits = c(2005, 2024), name = xlabel) +
    ggtitle(title) +
    ylab(ylabel)
  
  
  # Add the 2015 projections if provided
  if (!is.null(trajs.quants.2015)) {
    trajs.quants.2015.tmp <- t(trajs.quants.2015)
    trajs.quants.2015.transp <- data.frame(trajs.quants.2015.tmp[-1, ])
    names(trajs.quants.2015.transp) <- paste0("Quant", trajs.quants.2015.tmp[1,])
    trajs.quants.2015.transp$Year <- 2015:2024
    
    # Plot the 2015 projections with a different color
    color.us.2015 <- "red"  # 可以选择不同的颜色
    plot.obj <- plot.obj +
      geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.2015.transp,
                  alpha=0.3, fill=color.us.2015) +
      geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.2015.transp,
                  alpha=0.2, fill=color.us.2015) +
      geom_line(data=trajs.quants.2015.transp, aes(x=Year, y=Quant0.5,
                                                   color="Projections 2015"),
                alpha=1, size=1.3, color=color.us.2015)
  }
  
  # Process the primary trajs.quants data
  trajs.quants.tmp <- t(trajs.quants)
  trajs.quants.transp <- data.frame(trajs.quants.tmp[-1, ])
  names(trajs.quants.transp) <- paste0("Quant", trajs.quants.tmp[1,])
  trajs.quants.transp$Year <- 2024:2024
  
  # Plot the primary projections
  color.us <- "blue"
  plot.obj <- plot.obj +
    geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.transp,
                alpha=0.3, fill=color.us) +
    geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.transp,
                alpha=0.2, fill=color.us) 
  # +
  #   geom_line(data=trajs.quants.transp, aes(x=Year, y=Quant0.5,
  #                                           color="Projections"),
  #             alpha=1, size=1.3, color=color.us)
  
  
  
  # Plot historical data
  plot.obj <- plot.obj + geom_line(data=co2.sums.na.rm,
                                   aes(x=Year,y=Carbon))
  if (!is.null(additional_line)) {
    additional_line_df <- data.frame(
      Year = 2015:2024,
      CO2 = as.numeric(additional_line)
    )
    plot.obj <- plot.obj +
      geom_line(data=additional_line_df, aes(x=Year, y=CO2), color="green", size = 1.5)
  }
  plot.obj
}

plot.co2.projections.country <- function(data.medium.pre,
                                         data.medium.post,
                                 trajs.quants,
                                 plot.rcp=FALSE,
                                 ybreaks=NULL,
                                 trajs.quants.2015=NULL,
                                 title="Country CO2 Emissions",
                                 ylabel=NULL,
                                 xlabel = "(a)",
                                 additional_line=NULL) {  # Add an optional ylabel parameter, default is NULL
  
  # Required libraries
  library(doBy)
  library(ggplot2)
  
  data.restriction.pre <- data.medium.pre[!is.na(data.medium.pre$CO2),]
  data.restriction.post <- data.medium.post[!is.na(data.medium.post$CO2),]
  
  co2.sums.na.rm.pre <- summaryBy(CO2.total~ Year, data.restriction.pre, FUN=function(x) sum(x, na.rm=T))
  co2.sums.na.rm.pre <- co2.sums.na.rm.pre[co2.sums.na.rm.pre$Year >= 2005 & co2.sums.na.rm.pre$Year <= 2015, ]
  co2.sums.na.rm.post <- summaryBy(CO2.total~ Year, data.restriction.post, FUN=function(x) sum(x, na.rm=T))
  co2.sums.na.rm.post <- co2.sums.na.rm.post[co2.sums.na.rm.post$Year >= 2016, ]
  co2.sums.na.rm <- rbind(co2.sums.na.rm.pre, co2.sums.na.rm.post)
  
  names(co2.sums.na.rm)[2] <- "Carbon"
  co2.sums.na.rm$Carbon <- co2.sums.na.rm$Carbon / 1e3 * 11/3
  
  # Create base plot
  plot.obj <- ggplot() +
    labs(color="Scenario") +
    theme(panel.background=element_rect(fill="white"),
          panel.grid.minor=element_line(color="white"),
          panel.grid.major=element_line(color="grey90"),
          text=element_text(family="Arial", size=16),
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 12)) + # Center the title
    scale_y_continuous(breaks=pretty_breaks(n=4)) +
    scale_x_continuous(breaks=seq(2005, 2024, by=5), limits = c(2005, 2024), name = xlabel) +
    ggtitle(title) +
    ylab(ylabel)
  
  # If additional 2015 projections data is provided, plot it
  if (!is.null(trajs.quants.2015)) {
    trajs.quants.2015.transp <- data.frame(
      Year = 2015:2024,
      Quant0.025 = trajs.quants.2015$`0.025`$CO2[1:9]/10^9,
      Quant0.05 = trajs.quants.2015$`0.05`$CO2[1:9]/10^9,
      Quant0.5 = trajs.quants.2015$`0.5`$CO2[1:9]/10^9,
      Quant0.95 = trajs.quants.2015$`0.95`$CO2[1:9]/10^9,
      Quant0.975 = trajs.quants.2015$`0.975`$CO2[1:9]/10^9
    )
    
    # Plot the 2015 projections with a different color
    color.us.2015 <- "red"
    plot.obj <- plot.obj +
      geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.2015.transp,
                  alpha=0.3, fill=color.us.2015) +
      geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.2015.transp,
                  alpha=0.2, fill=color.us.2015) +
      geom_line(data=trajs.quants.2015.transp, aes(x=Year, y=Quant0.5, color="Projections 2015"),
                alpha=1, size=1.3, color=color.us.2015)
  }
  
  # Add ylabel if provided
  if (!is.null(ylabel)) {
    plot.obj <- plot.obj + ylab(ylabel)
  }
  
  
  # Plot historical data
  plot.obj <- plot.obj + geom_line(data=co2.sums.na.rm, aes(x=Year, y=Carbon))
  
  if (!is.null(additional_line)) {
    additional_line_df <- data.frame(
      Year = 2015:2024,
      CO2 = as.numeric(additional_line)/10^9
    )
    plot.obj <- plot.obj +
      geom_line(data=additional_line_df, aes(x=Year, y=CO2), color="green", size = 1.5)
  }
  
  plot.obj
}

#######################
#### Four subplots ####
#######################
# Generate each plot and store them in variables
additional_line_data <- proj.evals.2015.adjusted[["trajs.annual.quants.cont"]]["50%",2:10]
plot_world_co2 <- plot.co2.projections.world(data.medium, data_medium_wpp2024_updated2024,
                                   trajs.quants = proj.evals.2024.ar1.const$trajs.annual.quants[,1:10],
                                   trajs.quants.2015 = proj.evals.2015.ar1.const$trajs.annual.quants[,1:10],
                                   xlabel = "(a)",
                                   additional_line = additional_line_data)
plot_china_co2 <- plot.co2.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "CHN", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "CHN", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$CHN,
  plot.rcp = FALSE,
  ybreaks = seq(0, 30, by=6),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$CHN,
  title = "China",
  xlabel = "(b)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["CHN"]][["0.5"]]$CO2[1:9]
)
plot_usa_co2 <- plot.co2.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "USA", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "USA", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$USA,
  plot.rcp = FALSE,
  ybreaks = seq(0, 10, by=2),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$USA,
  title = "United States",
  xlabel = "(c)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["USA"]][["0.5"]]$CO2[1:9]
)
plot_germany_co2 <- plot.co2.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "DEU", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "DEU", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$DEU,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$DEU,
  title = "Germany",
  xlabel = "(d)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["DEU"]][["0.5"]]$CO2[1:9]
)

##### supplementary

plot_india_co2 <- plot.co2.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "IND", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "IND", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$IND,
  plot.rcp = FALSE,
  ybreaks = seq(0, 30, by=6),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$IND,
  title = "India",
  xlabel = "(a)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["IND"]][["0.5"]]$CO2[1:9]
)
plot_japan_co2 <- plot.co2.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "JPN", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "JPN", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$JPN,
  plot.rcp = FALSE,
  ybreaks = seq(0, 10, by=2),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$JPN,
  title = "Japan",
  xlabel = "(b)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["JPN"]][["0.5"]]$CO2[1:9]
)
plot_russia_co2 <- plot.co2.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "RUS", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "RUS", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$RUS,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$RUS,
  title = "Russia",
  xlabel = "(c)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["RUS"]][["0.5"]]$CO2[1:9]
)
plot_uk_co2 <- plot.co2.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "GBR", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "GBR", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$GBR,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$GBR,
  title = "United Kingdom",
  xlabel = "(d)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["GBR"]][["0.5"]]$CO2[1:9]
)


###############################
######### tech ################
###############################
plot.tech.projections.world <- function(data.medium.pre, data.medium.post,
                                       trajs.quants,
                                       plot.rcp=FALSE,
                                       ybreaks=seq(0,120,by=20),
                                       trajs.quants.2015=NULL,
                                       title="World",
                                       ylabel="Yearly Carbon Intensity",
                                       xlabel = "(a)",
                                       additional_line=NULL) {  # 添加一个新的参数来接受2015数据
  # plot.co2.projections plots projections of yearly CO2 emissions worldwide
  
  # Pre-2016 data
  data.restriction.pre <- data.medium.pre[!is.na(data.medium.pre$Tech),]
  data.restriction.pre$popCO2 <- data.restriction.pre$Pop * data.restriction.pre$CO2
  data.restriction.pre$popGDP <- data.restriction.pre$Pop * data.restriction.pre$GDP
  
  co2.sums.na.rm.pre <- data.restriction.pre %>%
    group_by(Year) %>%
    summarize(
      popCO2 = sum(popCO2, na.rm = TRUE),
      popGDP = sum(popGDP, na.rm = TRUE),
      Pop = sum(Pop, na.rm = TRUE),
      CO2 = sum(CO2, na.rm = TRUE),
      GDP = sum(GDP, na.rm = TRUE)
    )
  
  # 仅保留2005-2015年的数据
  co2.sums.na.rm.pre <- co2.sums.na.rm.pre[co2.sums.na.rm.pre$Year >= 1960 & co2.sums.na.rm.pre$Year <= 2015, ]
  
  # Post-2015 data
  data.restriction.post <- data.medium.post[!is.na(data.medium.post$Tech),]
  data.restriction.post$popCO2 <- data.restriction.post$Pop * data.restriction.post$CO2
  data.restriction.post$popGDP <- data.restriction.post$Pop * data.restriction.post$GDP
  
  co2.sums.na.rm.post <- data.restriction.post %>%
    group_by(Year) %>%
    summarize(
      popCO2 = sum(popCO2, na.rm = TRUE),
      popGDP = sum(popGDP, na.rm = TRUE),
      Pop = sum(Pop, na.rm = TRUE),
      CO2 = sum(CO2, na.rm = TRUE),
      GDP = sum(GDP, na.rm = TRUE)
    )
  
  # 仅保留2016年及之后的数据
  co2.sums.na.rm.post <- co2.sums.na.rm.post[co2.sums.na.rm.post$Year >= 2016, ]
  
  # 合并 pre 和 post 数据
  co2.sums.na.rm <- rbind(co2.sums.na.rm.pre, co2.sums.na.rm.post)
  
  # 计算 Tech 指标
  co2.sums.na.rm$Tech <- (co2.sums.na.rm$popCO2 / co2.sums.na.rm$Pop) * 10^4 / (co2.sums.na.rm$popGDP / co2.sums.na.rm$Pop)
  
  year.start <- 2010
  year.end <- 2024
  x.vals <- seq(year.start, year.end, by=5)
  
  # Create plot
  plot.obj <- ggplot() +
    labs(color="Scenario") +
    theme(panel.background=element_rect(fill="white"),
          panel.grid.minor=element_line(color="white"),
          panel.grid.major=element_line(color="grey90"),
          text=element_text(family="Arial", size=16),
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 12)) +  # Center the title
    scale_y_continuous(breaks=pretty_breaks(n=4)) +
    scale_x_continuous(breaks=seq(2005,2024,by=5), name = xlabel) +
    # ggtitle(title) +
    ylab(ylabel)
  
  
  # Add the 2015 projections if provided
  if (!is.null(trajs.quants.2015)) {
    trajs.quants.2015.transp = data.frame(
      tech025 = trajs.quants.2015[["0.025"]]$Tech[1:9],
      tech005 = trajs.quants.2015[["0.05"]]$Tech[1:9],
      tech05 = trajs.quants.2015[["0.5"]]$Tech[1:9],
      tech095 = trajs.quants.2015[["0.95"]]$Tech[1:9],
      tech0975 = trajs.quants.2015[["0.975"]]$Tech[1:9]
    )
    
    
    # trajs.quants.2015.tmp <- t(trajs.quants.2015.adj)
    # trajs.quants.2015.transp <- data.frame(trajs.quants.2015.tmp[-1, ])
    names(trajs.quants.2015.transp) <- paste0("Quant", c(0.025,0.050,0.500,0.950,0.975))
    trajs.quants.2015.transp$Year <- 2015:2024
    
    # Plot the 2015 projections with a different color
    color.us.2015 <- "red"  # 可以选择不同的颜色
    plot.obj <- plot.obj +
      geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.2015.transp,
                  alpha=0.3, fill=color.us.2015) +
      geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.2015.transp,
                  alpha=0.2, fill=color.us.2015) +
      geom_line(data=trajs.quants.2015.transp, aes(x=Year, y=Quant0.5,
                                                   color="Projections 2015"),
                alpha=1, size=1.3, color=color.us.2015)
  }
  
  
  # Plot historical data
  plot.obj <- plot.obj + geom_line(data=co2.sums.na.rm,
                                   aes(x=Year,y=Tech))
  if (!is.null(additional_line)) {
    additional_line_df <- data.frame(
      Year = 2015:2024,
      CO2 = as.numeric(additional_line)
    )
    plot.obj <- plot.obj +
      geom_line(data=additional_line_df, aes(x=Year, y=CO2), color="green", size = 1.5)
  }
  plot.obj
}


# country
plot.tech.projections.country <- function(data.medium.pre, data.medium.post,
                                         trajs.quants,
                                         plot.rcp=FALSE,
                                         ybreaks=NULL,
                                         trajs.quants.2015=NULL,
                                         title="Country Tech",
                                         ylabel=NULL,
                                         xlabel = "(a)",
                                         additional_line=NULL) {  # Add an optional ylabel parameter, default is NULL
  
  # Pre-2016 data
  data.restriction.pre <- data.medium.pre[!is.na(data.medium.pre$Tech),]
  
  co2.sums.na.rm.pre <- summaryBy(Tech ~ Year, data.restriction.pre, FUN=function(x) sum(x, na.rm=T))
  co2.sums.na.rm.pre <- co2.sums.na.rm.pre[co2.sums.na.rm.pre$Year >= 2005 & co2.sums.na.rm.pre$Year <= 2015, ]
  names(co2.sums.na.rm.pre)[2] <- "Tech"
  
  # Post-2015 data
  data.restriction.post <- data.medium.post[!is.na(data.medium.post$Tech),]
  
  co2.sums.na.rm.post <- summaryBy(Tech ~ Year, data.restriction.post, FUN=function(x) sum(x, na.rm=T))
  co2.sums.na.rm.post <- co2.sums.na.rm.post[co2.sums.na.rm.post$Year >= 2016, ]
  names(co2.sums.na.rm.post)[2] <- "Tech"
  
  # Combine pre and post data
  co2.sums.na.rm <- rbind(co2.sums.na.rm.pre, co2.sums.na.rm.post)
  
  # Create base plot
  plot.obj <- ggplot() +
    labs(color="Scenario") +
    theme(panel.background=element_rect(fill="white"),
          panel.grid.minor=element_line(color="white"),
          panel.grid.major=element_line(color="grey90"),
          text=element_text(family="Arial", size=16),
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 12)) + # Center the title
    scale_y_continuous(breaks=pretty_breaks(n=4)) +  # Dynamic y-axis breaks
    scale_x_continuous(breaks=seq(2005,2023,by=5), name=xlabel) +
    # ggtitle(title) +
    ylab(ylabel)
  
  # If additional 2015 projections data is provided, plot it
  if (!is.null(trajs.quants.2015)) {
    trajs.quants.2015.transp <- data.frame(
      Year = 2015:2024,
      Quant0.025 = trajs.quants.2015$`0.025`$Tech[1:9],
      Quant0.05 = trajs.quants.2015$`0.05`$Tech[1:9],
      Quant0.5 = trajs.quants.2015$`0.5`$Tech[1:9],
      Quant0.95 = trajs.quants.2015$`0.95`$Tech[1:9],
      Quant0.975 = trajs.quants.2015$`0.975`$Tech[1:9]
    )
    
    # Plot the 2015 projections with a different color
    color.us.2015 <- "red"
    plot.obj <- plot.obj +
      geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.2015.transp,
                  alpha=0.3, fill=color.us.2015) +
      geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.2015.transp,
                  alpha=0.2, fill=color.us.2015) +
      geom_line(data=trajs.quants.2015.transp, aes(x=Year, y=Quant0.5, color="Projections 2015"),
                alpha=1, size=1.3, color=color.us.2015)
  }
  
  # Add ylabel if provided
  if (!is.null(ylabel)) {
    plot.obj <- plot.obj + ylab(ylabel)
  }
  
  
  # Plot historical data
  plot.obj <- plot.obj + geom_line(data=co2.sums.na.rm, aes(x=Year, y=Tech))
  
  if (!is.null(additional_line)) {
    additional_line_df <- data.frame(
      Year = 2015:2024,
      CO2 = as.numeric(additional_line)
    )
    plot.obj <- plot.obj +
      geom_line(data=additional_line_df, aes(x=Year, y=CO2), color="green", size = 1.5)
  }
  
  plot.obj
}

# Generate each plot and store them in variables
tech_values <- sapply(proj.evals.2015.adjusted[["trajs.annual.worldwide.cont"]], function(df) df$Tech)
tech_medians_per_year <- apply(tech_values, 1, median)[1:10]  # 2015-2024
additional_line_data <- tech_medians_per_year
plot_world_tech <- plot.tech.projections.world(data.medium, data_medium_wpp2024_updated2024,
                                         trajs.quants = proj.evals.2024.ar1.const$trajs.annual.quants[,1:10], # ?
                                         trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles"]],
                                         xlabel = "(e)",
                                         additional_line = additional_line_data)

plot_china_tech <- plot.tech.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "CHN", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "CHN", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$CHN,
  plot.rcp = FALSE,
  ybreaks = seq(0, 30, by=6),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]][["CHN"]],
  title = "China",
  xlabel = "(f)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["CHN"]][["0.5"]]$Tech[1:9]
)

plot_usa_tech <- plot.tech.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "USA", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "USA", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$USA,
  plot.rcp = FALSE,
  ybreaks = seq(0, 10, by=2),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]][["USA"]],
  title = "USA",
  xlabel = "(g)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["USA"]][["0.5"]]$Tech[1:9]
)

plot_germany_tech <- plot.tech.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "DEU", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "DEU", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$DEU,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]][["DEU"]],
  title = "Germany",
  xlabel = "(h)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["DEU"]][["0.5"]]$Tech[1:9]
)

##### supplementary

plot_india_tech <- plot.tech.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "IND", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "IND", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$IND,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]][["IND"]],
  title = "India",
  xlabel = "(e)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["IND"]][["0.5"]]$Tech[1:9]
)

plot_japan_tech <- plot.tech.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "JPN", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "JPN", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$JPN,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]][["JPN"]],
  title = "Japan",
  xlabel = "(f)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["JPN"]][["0.5"]]$Tech[1:9]
)

plot_russia_tech <- plot.tech.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "RUS", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "RUS", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$RUS,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]][["RUS"]],
  title = "Russia",
  xlabel = "(g)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["RUS"]][["0.5"]]$Tech[1:9]
)

plot_uk_tech <- plot.tech.projections.country(
  data.medium.pre = data.medium[data.medium$Isocode == "GBR", ],
  data.medium.post = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "GBR", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$GBR,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]][["GBR"]],
  title = "United Kingdom",
  xlabel = "(h)", additional_line = proj.evals.2015.adjusted[["ipat.annual.quantiles.bycountry"]][["GBR"]][["0.5"]]$Tech[1:9]
)


##### plot

# combined_plot <- plot_grid(
#   plot_world_tech, plot_usa_tech, plot_china_tech, plot_germany_tech,
#   hjust = 0.5,                             # 水平居中对齐标签
#   vjust = 0.5,
#   nrow = 2, ncol = 2, align = "hv"
# )
# 
# final_plot <- ggdraw() +
#   draw_plot(combined_plot, x = 0.03, y = 0.03, width = 0.94, height = 0.94) +  # 仅略微缩小图表
#   draw_label("Year", x = 0.5, y = 0, vjust = -1, size = 22) +
#   draw_label("Yearly Carbon Intensity (0.1 kg CO2/2011$ GDP)", x = 0, y = 0.5, angle = 90, vjust = 1.5, size = 22)

###################
## final combine ##
###################
# without legend
plot_grid(plot_world_co2, plot_china_co2, plot_usa_co2, plot_germany_co2, 
          plot_world_tech, plot_china_tech, plot_usa_tech, plot_germany_tech, 
          nrow = 2)
# with legend
# 创建一个单独的图例，并反转显示顺序
legend_plot <- ggplot(data.frame(x = c(1, 2, 3), y = c(1, 1, 1), group = c("Continued NDC", "Medium proj", "real data")),
                      aes(x = x, y = y, color = group)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("Continued NDC" = "green", "Medium proj" = "red", "real data" = "black"),
    labels = c("Continued NDC" = "Continued NDC", "Medium proj" = "Medium Proj", "real data" = "Real Data")
  ) +
  theme_void() +  # 移除背景网格和轴
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# 提取图例
legend <- cowplot::get_legend(legend_plot)

# 整合主图和图例
plot_grid(
  plot_grid(plot_world_co2, plot_china_co2, plot_usa_co2, plot_germany_co2,
            plot_world_tech, plot_china_tech, plot_usa_tech, plot_germany_tech,
            nrow = 2),
  legend,
  rel_widths = c(7, 1)  # 调整主图和图例的宽度比例
)

#### supplementary

# without legend
plot_grid(plot_india_co2, plot_japan_co2, plot_russia_co2, plot_uk_co2, 
          plot_india_tech, plot_japan_tech, plot_russia_tech, plot_uk_tech, 
          nrow = 2)
# with legend
# 创建一个单独的图例，并反转显示顺序
legend_plot <- ggplot(data.frame(x = c(1, 2, 3), y = c(1, 1, 1), group = c("Continued NDC", "Medium proj", "real data")),
                      aes(x = x, y = y, color = group)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("Continued NDC" = "green", "Medium proj" = "red", "real data" = "black"),
    labels = c("Continued NDC" = "Continued NDC", "Medium proj" = "Medium Proj", "real data" = "Real Data")
  ) +
  theme_void() +  # 移除背景网格和轴
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# 提取图例
legend <- cowplot::get_legend(legend_plot)

# 整合主图和图例
plot_grid(
  plot_grid(plot_india_co2, plot_japan_co2, plot_russia_co2, plot_uk_co2, 
            plot_india_tech, plot_japan_tech, plot_russia_tech, plot_uk_tech,
            nrow = 2),
  legend,
  rel_widths = c(7, 1)  # 调整主图和图例的宽度比例
)


#################################################
#################### figure2 ####################
#################################################

plot.co2.projections.world <- function(data.medium,
                                 trajs.quants,
                                 plot.rcp=FALSE,
                                 ybreaks=seq(0,120,by=20),
                                 trajs.quants.2015=NULL,
                                 title="World",
                                 ylabel=NULL,
                                 xlabel = "(a)") {  # 添加一个新的参数来接受2015数据
  # plot.co2.projections plots projections of yearly CO2 emissions worldwide
  
  library(doBy)
  library(ggplot2)
  data.restriction <- data.medium[!is.na(data.medium$CO2),]
  
  co2.sums.na.rm <- summaryBy(CO2.total~ Year, data.restriction, FUN=function(x) sum(x, na.rm=T))
  names(co2.sums.na.rm)[2] <- "Carbon"
  co2.sums.na.rm$Carbon <- co2.sums.na.rm$Carbon / 1e3 * 11/3
  
  year.start <- 2010
  year.end <- 2100
  x.vals <- seq(year.start, year.end, by=5)
  
  # Create plot
  plot.obj <- ggplot() +
    labs(color="Scenario") +
    theme(panel.background=element_rect(fill="white"),
          panel.grid.minor=element_line(color="white"),
          panel.grid.major=element_line(color="grey90"),
          text=element_text(family="Arial", size=18),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 12)) +  # Center the title
    scale_y_continuous(breaks=ybreaks) +
    scale_x_continuous(breaks=seq(1960,2100,by=20), name = xlabel) +
    ggtitle(title) +
    ylab(ylabel)
  
  
  # Add the 2015 projections if provided
  if (!is.null(trajs.quants.2015)) {
    trajs.quants.2015.tmp <- t(trajs.quants.2015)
    trajs.quants.2015.transp <- data.frame(trajs.quants.2015.tmp[-1, ])
    names(trajs.quants.2015.transp) <- paste0("Quant", trajs.quants.2015.tmp[1,])
    trajs.quants.2015.transp$Year <- 2015:2100
    
    # Plot the 2015 projections with a different color
    color.us.2015 <- "#61c75b" # 可以选择不同的颜色
    plot.obj <- plot.obj +
      geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.2015.transp,
                  alpha=0.3, fill=color.us.2015) +
      # geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.2015.transp,
      #             alpha=0.2, fill=color.us.2015) +
      geom_line(data=trajs.quants.2015.transp, aes(x=Year, y=Quant0.5,
                                                   color="Projections 2015"),
                alpha=1, size=1.3, color=color.us.2015)
  }
  
  # Process the primary trajs.quants data
  trajs.quants.tmp <- t(trajs.quants)
  trajs.quants.transp <- data.frame(trajs.quants.tmp[-1, ])
  names(trajs.quants.transp) <- paste0("Quant", trajs.quants.tmp[1,])
  trajs.quants.transp$Year <- 2024:2100
  
  # Plot the primary projections
  color.us <- "#e41a1c"
  plot.obj <- plot.obj +
    geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.transp,
                alpha=0.3, fill=color.us) +
    # geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.transp,
    #             alpha=0.2, fill=color.us) +
    geom_line(data=trajs.quants.transp, aes(x=Year, y=Quant0.5,
                                            color="Projections"),
              alpha=1, size=1.3, color=color.us)
  
  
  
  # Plot historical data
  plot.obj <- plot.obj + geom_line(data=co2.sums.na.rm,
                                   aes(x=Year,y=Carbon))
  
  plot.obj
}


# country
plot.co2.projections.country <- function(data.medium,
                                 trajs.quants,
                                 plot.rcp=FALSE,
                                 ybreaks=NULL,
                                 trajs.quants.2015=NULL,
                                 title="Country CO2 Emissions Projection",
                                 ylabel=NULL,
                                 xlabel = "(a)") {  # Add an optional ylabel parameter, default is NULL
  
  # Required libraries
  library(doBy)
  library(ggplot2)
  
  # Filter historical data for missing values
  data.restriction <- data.medium[!is.na(data.medium$CO2),]
  
  # Summarize CO2 for historical data
  co2.sums.na.rm <- summaryBy(CO2.total ~ Year, data.restriction, FUN=function(x) sum(x, na.rm=T))
  names(co2.sums.na.rm)[2] <- "Carbon"
  co2.sums.na.rm$Carbon <- co2.sums.na.rm$Carbon / 1e3 * 11/3
  
  # Create base plot
  plot.obj <- ggplot() +
    labs(color="Scenario") +
    theme(panel.background=element_rect(fill="white"),
          panel.grid.minor=element_line(color="white"),
          panel.grid.major=element_line(color="grey90"),
          text=element_text(family="Arial", size=18),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 12)) + # Center the title
    scale_y_continuous(breaks=ybreaks) +  # Dynamic y-axis breaks
    scale_x_continuous(breaks=seq(1960,2100,by=20), name=xlabel) +
    ggtitle(title) +
    ylab(ylabel)
  
  # If additional 2015 projections data is provided, plot it
  if (!is.null(trajs.quants.2015)) {
    trajs.quants.2015.transp <- data.frame(
      Year = 2015:2100,
      Quant0.025 = trajs.quants.2015$`0.025`$CO2/10^9,
      Quant0.05 = trajs.quants.2015$`0.05`$CO2/10^9,
      Quant0.5 = trajs.quants.2015$`0.5`$CO2/10^9,
      Quant0.95 = trajs.quants.2015$`0.95`$CO2/10^9,
      Quant0.975 = trajs.quants.2015$`0.975`$CO2/10^9
    )
    
    # Plot the 2015 projections with a different color
    color.us.2015 <- "#61c75b"
    plot.obj <- plot.obj +
      geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.2015.transp,
                  alpha=0.3, fill=color.us.2015) +
      # geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.2015.transp,
      #             alpha=0.2, fill=color.us.2015) +
      geom_line(data=trajs.quants.2015.transp, aes(x=Year, y=Quant0.5, color="Projections 2015"),
                alpha=1, size=1.3, color=color.us.2015)
  }
  
  # Add ylabel if provided
  if (!is.null(ylabel)) {
    plot.obj <- plot.obj + ylab(ylabel)
  }
  
  # Transform trajs.quants data for plotting
  trajs.quants.transp <- data.frame(
    Year = 2024:2100, 
    Quant0.025 = trajs.quants$`0.025`$CO2/10^9, # in gigatonne
    Quant0.05 = trajs.quants$`0.05`$CO2/10^9,
    Quant0.5 = trajs.quants$`0.5`$CO2/10^9,
    Quant0.95 = trajs.quants$`0.95`$CO2/10^9,
    Quant0.975 = trajs.quants$`0.975`$CO2/10^9
  )
  
  # Plot projections based on quantiles
  color.us <- "#e41a1c"
  plot.obj <- plot.obj +
    geom_ribbon(aes(x=Year, ymin=Quant0.05, ymax=Quant0.95), data=trajs.quants.transp,
                alpha=0.3, fill=color.us) + # 90%CI
    # geom_ribbon(aes(x=Year, ymin=Quant0.025, ymax=Quant0.975), data=trajs.quants.transp,
    #             alpha=0.2, fill=color.us) +
    geom_line(data=trajs.quants.transp, aes(x=Year, y=Quant0.5, color="Projections"),
              alpha=1, size=1.3, color=color.us)
  
  
  # Plot historical data
  plot.obj <- plot.obj + geom_line(data=co2.sums.na.rm, aes(x=Year, y=Carbon))
  
  plot.obj
}



# Generate each plot and store them in variables
plot_world <- plot.co2.projections.world(data_medium_wpp2024_updated2024,
                                   trajs.quants = proj.evals.2024.ar1.const$trajs.annual.quants,
                                   trajs.quants.2015 = proj.evals.2015.ar1.const$trajs.annual.quants,
                                   xlabel = "(a)") + scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20))


plot_usa <- plot.co2.projections.country(
  data.medium = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "USA", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$USA,
  plot.rcp = FALSE,
  ybreaks = seq(0, 10, by=2),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$USA,
  title = "USA",
  xlabel = "(c)"
)+ scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 4))

plot_china <- plot.co2.projections.country(
  data.medium = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "CHN", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$CHN,
  plot.rcp = FALSE,
  ybreaks = seq(0, 30, by=6),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$CHN,
  title = "China",
  xlabel = "(b)"
)+ scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 10))

plot_germany <- plot.co2.projections.country(
  data.medium = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "DEU", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$DEU,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$DEU,
  title = "Germany",
  xlabel = "(d)"
)+ scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.4))

####### supplementary

plot_india <- plot.co2.projections.country(
  data.medium = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "IND", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$IND,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$IND,
  title = "India",
  xlabel = "(a)"
)+ scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.4))

plot_japan <- plot.co2.projections.country(
  data.medium = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "JPN", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$JPN,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$JPN,
  title = "Japan",
  xlabel = "(b)"
)+ scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.4))

plot_russia <- plot.co2.projections.country(
  data.medium = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "RUS", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$RUS,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$RUS,
  title = "Russia",
  xlabel = "(c)"
)+ scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.4))

plot_uk <- plot.co2.projections.country(
  data.medium = data_medium_wpp2024_updated2024[data_medium_wpp2024_updated2024$Isocode == "GBR", ],
  trajs.quants = proj.evals.2024.ar1.const[["ipat.annual.quantiles.bycountry"]]$GBR,
  plot.rcp = FALSE,
  ybreaks = seq(0, 2, by=0.3),
  trajs.quants.2015 = proj.evals.2015.ar1.const[["ipat.annual.quantiles.bycountry"]]$GBR,
  title = "United Kingdom",
  xlabel = "(d)"
)+ scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.4))


# plot
# Arrange the plots in a 2x2 grid
#grid.arrange(plot_world, plot_usa, plot_china, plot_germany, nrow = 2, ncol = 2)

# 创建 2x2 布局的组合图
combined_plot <- plot_grid(
  plot_world, plot_china, plot_usa, plot_germany,
  hjust = 0.5,                             # 水平居中对齐标签
  vjust = 0.5,
  nrow = 2, ncol = 2, align = "hv"
)
# 创建一个单独的图例，并反转显示顺序
legend_plot <- ggplot(data.frame(x = c(1, 2, 3), y = c(1, 1, 1), group = c("Medium proj 2015", "Medium proj 2024", "real data")),
                      aes(x = x, y = y, color = group)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("Medium proj 2015" = "#61c75b", "Medium proj 2024" = "red", "real data" = "black"),
    labels = c("Medium proj 2015" = "Proj 2015", "Medium proj 2024" = "Proj 2024", "real data" = "Real Data")
  ) +
  theme_void() +  # 移除背景网格和轴
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# 提取图例
legend <- cowplot::get_legend(legend_plot)

# 在 2x2 组合图的四周添加 plot_spacer()，形成额外的边界空间
final_plot <- ggdraw() +
  draw_plot(combined_plot, x = 0.03, y = 0.03, width = 0.94, height = 0.94) +  # 仅略微缩小图表
  draw_label("Year", x = 0.5, y = 0, vjust = -1, size = 18) +
  draw_label("Yearly CO2 emissions (gt CO2)", x = 0, y = 0.5, angle = 90, vjust = 1.5, size = 18)

final_plot = plot_grid(final_plot,legend,
                       rel_widths = c(10, 1))
# 显示最终图表
print(final_plot)

########### supplementary

# 创建 2x2 布局的组合图
combined_plot <- plot_grid(
  plot_india, plot_japan, plot_russia, plot_uk,
  hjust = 0.5,                             # 水平居中对齐标签
  vjust = 0.5,
  nrow = 2, ncol = 2, align = "hv"
)
# 创建一个单独的图例，并反转显示顺序
legend_plot <- ggplot(data.frame(x = c(1, 2, 3), y = c(1, 1, 1), group = c("Medium proj 2015", "Medium proj 2024", "real data")),
                      aes(x = x, y = y, color = group)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c("Medium proj 2015" = "#61c75b", "Medium proj 2024" = "red", "real data" = "black"),
    labels = c("Medium proj 2015" = "Proj 2015", "Medium proj 2024" = "Proj 2024", "real data" = "Real Data")
  ) +
  theme_void() +  # 移除背景网格和轴
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# 提取图例
legend <- cowplot::get_legend(legend_plot)

# 在 2x2 组合图的四周添加 plot_spacer()，形成额外的边界空间
final_plot <- ggdraw() +
  draw_plot(combined_plot, x = 0.03, y = 0.03, width = 0.94, height = 0.94) +  # 仅略微缩小图表
  draw_label("Year", x = 0.5, y = 0, vjust = -1, size = 18) +
  draw_label("Yearly CO2 emissions (gt CO2)", x = 0, y = 0.5, angle = 90, vjust = 1.5, size = 18)

final_plot = plot_grid(final_plot,legend,
                       rel_widths = c(10, 1))
# 显示最终图表
print(final_plot)



#################################################
#################### figure3 ####################
#################################################
plot_temp_none <- function(summary_proj, summary_proj2015, summary_anomaly, title = 'Assembled Global Mean Temperature Forecast')
{
  
  summary_anomaly <- subset(summary_anomaly, year >= 1960)
  summary_proj_none <- subset(summary_proj, adjusted == "None")
  summary_proj_adjusted <- subset(summary_proj, adjusted == "Adjusted")
  summary_proj_continued <- subset(summary_proj, adjusted == "Continued")
  summary_proj_usa_excluded <- subset(summary_proj, adjusted == "USA Excluded")
  
  if (!is.null(summary_proj2015)) {
    summary_proj2015_none <- subset(summary_proj2015, adjusted == "None")
  }
  
  other_estimates <- data.frame(year = 2006:2024, observed_anomaly = real_data$V6[157:175] - mean(real_data$V6[12:31])) # change to 174 due to 2023
  other_quantiles <- other_estimates$observed_anomaly +
    matrix(rep(qnorm(c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)), each = 18), nrow=18) # change to 18
  other_quantiles <- data.frame(other_quantiles)
  names(other_quantiles) <- c("q025", "q050", "q100", "median", "q900", "q950", "q975")
  other_estimates <- cbind(other_estimates, other_quantiles)
  summary_anomaly <- rbind(summary_anomaly, other_estimates)

  p1 <- ggplot(data = summary_proj_none, aes(x = year))  +
    theme(text=element_text(size=17),
          panel.background = element_rect(fill="white"),
          panel.grid.major = element_line(color="white"),
          panel.grid.minor = element_line(color="white"),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 45),
          legend.title = element_blank()) +
    geom_ribbon(data = summary_proj2015_none, aes(x = year, ymin = q050, ymax = q950), fill = 'darkgreen', alpha = 0.3) +
    geom_line(data = summary_proj2015_none, aes(x = year, y = median, color = 'LR21 Proj'), size = 1.2)+
    geom_hline(yintercept = seq(0,4, by = 0.5), color = 'grey') +
    geom_vline(xintercept = c(2050,2100), color = 'grey') +
    geom_vline(xintercept = 2025, linetype = "dashed", color = "red", size = 1) +  # 添加2023年的竖线
    annotate("text", x = 2025, y = max(summary_proj_none$median), label = "2024",
             color = "red", vjust = 1.5, size = 5) +  # 在竖线附近添加“2023”的标签
    geom_vline(xintercept = 2016, linetype = "dashed", color = "darkgreen", size = 1) +  # 添加2023年的竖线
    annotate("text", x = 2016, y = max(summary_proj_none$median), label = "2016",
             color = "darkgreen", vjust = -2.5, size = 5) +
    geom_line(mapping = aes(x = year, y = median, color = 'None (New Proj)'), data = summary_proj_none, size = 1.2) +
    geom_line(mapping = aes(x = year, y = median, color = 'Adjusted'), data = summary_proj_adjusted, size = 1.2, linetype = "dashed") +
    geom_line(mapping = aes(x = year, y = median, color = 'Continued'), data = summary_proj_continued, size = 1.2, linetype = "dotted") +
    geom_line(mapping = aes(x = year, y = median, color = 'USA Excluded'), data = summary_proj_usa_excluded, size = 1.2, linetype = "dotdash") # 将Projection线设置为红色

  
    p1 <- p1 +
    geom_line(data = summary_anomaly, aes(x = year, y = observed_anomaly, color = 'HadCrut5')) +
    ylim(-0.5, 4.5) +
    scale_x_continuous(limits = c(1960, 2100), breaks = c(1960, 2000, 2050, 2100))  +  # 设置 x 轴范围
    scale_y_continuous(limits = c(0, 4))  

  # 添加 95% CI（红色）
  p1 <- p1 +
    # geom_ribbon(aes(ymin = q025, ymax = q975), fill = 'red', alpha = 0.2) +
    # 添加 90% CI（红色，稍微透明）
    geom_ribbon(aes(ymin = q050, ymax = q950), fill = 'red', alpha = 0.3) +
    xlab('year') + ylab('Anomaly') + ggtitle(title) +
    scale_color_manual(values = c(
      'HadCrut5' = 'black', 
      'LR21 Proj' = 'green', 
      'None (New Proj)' = 'red', 
      'Adjusted' = 'blue', 
      'Continued' = 'purple', 
      'USA Excluded' = 'orange'
    )) +
    scale_linetype_manual(values = c(
      'HadCrut5' = 'solid', 
      'LR21 Proj' = 'solid', 
      'None (New Proj)' = 'solid', 
      'Adjusted' = 'dashed', 
      'Continued' = 'dotted', 
      'USA Excluded' = 'dotdash'
    ))

  return (p1)
}

# 调用函数绘制图形
plot_temp_none(combined2023, combined2015 , summary_anomaly)
