
###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha
# outputSummaryLandscape <- get(load("outputSummaryLandscape.RData"))
outputSummaryLandscape <- get(load("../outputsCompiled/outputSummaryLandscape.RData"))


require(ggplot2)
require(dplyr)
require(tidyr)

### pools
df <- outputSummaryLandscape %>%
    filter(Time >=1,
           tenure == "Privé",
           variable %in% c("ABio",  "BBio", "TotalDOM"),
           treatment %in% c("firewoodSinglePass", "noFirewood")) %>%
    mutate(group = paste(mgmtAreaName, treatment))
    


png(filename= paste0("pools_Summary.png"),
    width = 7, height = 8, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = Time, y = value*unitConvFact, group = group,
              #linetype = tenure,
              colour = treatment)) +
    theme_dark() +
    facet_grid(MRC ~ variable  ) +
    #facet_wrap(~ variable  ) +
    geom_line() +
    scale_color_manual(values = c(firewoodSinglePass = "darkgoldenrod1",#"firebrick1",
                                  noFirewood = "cyan")) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0)) +
    labs(title = "Summary of aggregated pools",
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")),
         caption = paste0("All treatments include baseline clearcut logging and partial cutting based on each region's recent history.",
                          "\nABio : Aboveground biomass stocks.",
                          "\nBBio : Belowground (root) biomass stocks.",
                          "\nTotalDOM : Total dead organic matter and soil stocks."))


dev.off()


### stacked (total)
png(filename= paste0("pools_Stacked.png"),
    width = 6, height = 8, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = Time, y = value*unitConvFact,# group = group,
               #linetype = tenure,
               fill = variable)) + 
    stat_summary(fun.y="sum", geom="area", position = "stack") +
    facet_grid(MRC ~ treatment) +
    scale_fill_manual(values = c("forestgreen","chocolate2", "coral4" )) +
    theme_dark() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0)) +
    labs(title = "Evolution of ecosystem carbon stocks",
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")),
         caption = paste0("All treatments include baseline clearcut logging and partial cutting based on each region's recent history.",
                          "\nABio : Aboveground biomass stocks.",
                          "\nBBio : Belowground (root) biomass stocks.",
                          "\nTotalDOM : Total dead organic matter and soil stocks."))
dev.off()


### fluxes
df <- outputSummaryLandscape %>%
    filter(#tenure == "Privé",
        Time >=1,   
        variable %in% c("DelBio",  "Turnover",
                           "NetGrowth",  "NPP",
                           "Rh",  "NEP",
                           "NBP"),
           treatment %in% c("firewoodSinglePass", "noFirewood")) %>%
    mutate(group = paste(mgmtAreaName, treatment))


png(filename= paste0("fluxes_Summary.png"),
    width = 14, height = 8, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = Time, y = value*unitConvFact, group = group,
               linetype = tenure,
               colour = treatment)) +
    facet_grid(MRC ~ variable  ) +
    theme_dark() +
    geom_hline(yintercept = 0, linetype = 1, color = "grey35", size = 0.35) +
    geom_line() +
    scale_color_manual(values = c(firewoodSinglePass = "darkgoldenrod1",#"firebrick1",
                                  noFirewood = "cyan")) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0)) +
    labs(title = "Summary of global fluxes",
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1", " yr"^"-1", "\n")),
         caption = paste0("All treatments include baseline clearcut logging and partial cutting based on each region's recent history.",
                     "\nDelBio: Annual change in biomass stocks",
                     "\nTurnover: Annual transfer of biomass (above-and belowground) to dead organic matter and soil pools before disturbances occur",
                     "\nNetGrowth: Change in biomass from growth alone: the difference between the biomass at the beginning and the end of the growth routine in the timestep. This value could be negative as the stand ages and mortality outpaces growth. DelBio and NetGrowth will be the same when there are no losses caused by disturbances.",
                     "\nNPP : Net Primary Production (includes above and belowground). This includes growth and replacement of litterfall and annual turnover, i.e., the sum of NetGrow and turnover.",
                     "\nRh : Heterotrophic respiration. This is the sum of the “To Air” fluxes through decomposition, not disturbance.",
                     "\nNEP : Net Ecosystem Productivity. NPP minus Rh.",
                     "\nNBP : Net Biome Productivity. NEP minus losses from the ecosystem due to disturbances (both emissions to air from combustion and losses to the forest products sector.)"
                     ))

dev.off()


### focus on NEP (with firewood harvesting minus without firewood harvesting)

### fluxes
df <- outputSummaryLandscape %>%
    filter(tenure == "Privé",
           Time >=5,
           variable %in% c("NEP", "NBP"),
           treatment %in% c("firewoodSinglePass", "noFirewood"))

dfArea <- distinct(df[, c("MRC", "tenure", "area_ha")])

dfFW <- df %>%
    spread(variable, value) %>%
    mutate(toFPS = NBP - NEP) %>%
    spread(treatment, toFPS) %>%
    group_by(MRC, Time) %>%
    summarise(firewoodBurning = -(max(firewoodSinglePass, na.rm = T)
              - max(noFirewood, na.rm = T))) %>%
    mutate(firewoodBurning = ifelse(firewoodBurning <= 0, 0, firewoodBurning))

dfEmissions <- df %>%
    filter(variable == "NEP") %>%
    group_by(MRC, Time) %>%
    summarize(NEPgain = value[which(treatment == "noFirewood")] -
                  value[which(treatment == "firewoodSinglePass")]) %>%
    
    merge(dfFW) %>%
    #mutate(NEPgain = ifelse(firewoodBurning <= 0, 0, NEPgain)) %>%
    gather(key = "variable", value = "value", -MRC, -Time)

dfEmissions <- dfEmissions %>%
    spread(variable, value) %>%
    mutate(netEmissions = firewoodBurning + NEPgain) %>%
    gather(key = "variable", value = "value", -MRC, -Time, -netEmissions) %>%
    merge(dfArea)


varLvls <- c(firewoodBurning = "Firewood burning",
             NEPgain = "NEP gain")
dfEmissions$variable  <- factor(varLvls[match(dfEmissions$variable, names(varLvls))], levels = varLvls)
labDf <- distinct(dfEmissions[,c("MRC", "area_ha")])
labDf[,"lab"] <- paste("Total area (private tenure) :", labDf$area_ha, "ha")


#####################
png(filename= paste0("emissions_Firewood.png"),
    width = 10, height = 5, units = "in", res = 600, pointsize=10)

options(scipen=999)

ggplot() + 
    stat_summary(data = dfEmissions,
                 aes(x = Time, y = value*unitConvFact*area_ha,
                     fill = variable),
                 fun.y="sum", geom="area", position = "stack") +
    scale_fill_manual("", values = c("firebrick2", "darkolivegreen2")) +
    facet_wrap(~ MRC, ncol = 3) +
    #ylim(-50000, 120000) +
    geom_line(data = dfEmissions,
              aes(x = Time, y = netEmissions*unitConvFact*area_ha,
                  colour = tenure)) +
    scale_colour_manual("", values = "white", label = "Net emissions") +
    theme_dark() +
    geom_text(data = labDf, aes(x = 100,
                                y = 35000, #max(dfEmissions$value * unitConvFact* dfEmissions$area_ha),
                                label = lab),
              hjust = 1, size = 2) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text = element_text(size = rel(.75))) +
    labs(title = "Carbon emissions associated with residential firewood",
         #subtitle = "*Firewood harvesting is estimated",
         x = "",
         y = expression(paste("Emissions (tonnes C", " y"^"-1", ")", "\n")))
dev.off()

