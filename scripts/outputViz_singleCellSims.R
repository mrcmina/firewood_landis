
###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(ggplot2)
require(dplyr)
require(tidyr)


simArea <- c("MRCOuta")
treatment <- c("Firewood", "CJ", "CPRS")

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha

df <- dfPercentiles <- list()
for (t in seq_along(treatment)) {
    
    treat <- treatment[t]
    
    # log_BiomassC <- get(load(paste0("../outputsCompiled/log_BiomassC_",
    #                                 simArea, "_", treat, ".RData")))
    # log_Pools <- get(load(paste0("../outputsCompiled/log_Pools_",
    #                              simArea, "_", treat, ".RData")))
    log_Summary <- get(load(paste0("../outputsCompiled/log_Summary_",
                                   simArea, "_", treat, ".RData")))
    
    ### pools
    dfTmp <- log_Summary %>%
        filter(variable %in% c("ABio",  "BBio", "TotalDOM")) %>%
        mutate(group = paste(simID, areaName, treat,
                             initComm, landtype, replicate, variable))
    
   
        
    dfPercentilesTmp <- dfTmp %>%
        group_by(areaName, treatment,
                 initComm, landtype, Time, variable) %>%
        summarise(p25 = quantile(value, 0.25),
                  p50 = quantile(value, 0.5),
                  p75 = quantile(value, 0.75),
                  maxVal = max(value),
                  minVal = min(value))
    
    df[[t]] <- dfTmp
    dfPercentiles[[t]] <- dfPercentilesTmp
}

df <- do.call("rbind", df)
dfPercentiles <- do.call("rbind", dfPercentiles)

nSims <- length(unique(dfTmp$replicate))
#################
p <- ggplot(df, aes(x = Time, y = value*unitConvFact,
                    colour = variable, fill = variable)) +
    #linetype = tenure, +
    theme_dark() +
    facet_grid(landtype ~ treatment) +
    geom_line(data = dfPercentiles,
              aes(x = Time, y = p50*unitConvFact),
              size  = 0.5, alpha = 1) +
    geom_ribbon(data = dfPercentiles,
                aes(x = Time, y = NULL, colour = NULL,
                    ymin = minVal*unitConvFact, ymax = maxVal*unitConvFact),
                alpha = 0.25) +
    geom_ribbon(data = dfPercentiles,
                aes(x = Time, y = NULL, colour = NULL,
                    ymin = p25*unitConvFact, ymax = p75*unitConvFact),
                alpha = 0.5) +
    geom_line(data = filter(df, replicate %in% sample(unique(df$replicate), 1)),
              aes(x = Time, y = value*unitConvFact, group = group),
              size  = 0.2, alpha = 1, colour = "black") +
    scale_colour_manual(values = c("darkgreen","chocolate2", "coral4" )) +
    scale_fill_manual(values = c("darkgreen","chocolate2", "coral4" )) +
    theme(plot.caption = element_text(size = rel(.75), hjust = 0),
          plot.subtitle = element_text(size = rel(.75))) +
    labs(title = "Summary of aggregated pools (single-cell simulations)",
         subtitle = paste0("Medians are represented with coloured lines",
                           "\nEntire value ranges are represented with lightly shaded areas (", nSims, " replicates)",
                           "\n25th and 75th percentiles are represented with darker shaded areas.",
                           "\nA random simulation is represented in black"),
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")),
         caption = paste0("ABio : Aboveground biomass stocks.",
                          "\nBBio : Belowground (root) biomass stocks.",
                          "\nTotalDOM : Total dead organic matter and soil stocks."))

fName <- paste0("pools_Summary_singleCellSims_",
                simArea, ".png")

png(filename= fName,
    width = 10, height = 6, units = "in", res = 600, pointsize=10)

    print(p)

dev.off()
