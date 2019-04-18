
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
log_BiomassC <- get(load("../outputsCompiled/log_BiomassC_MRCOuta_CJ.RData"))
log_Pools <- get(load("../outputsCompiled/log_Pools_MRCOuta_CJ.RData"))
log_Summary <- get(load("../outputsCompiled/log_Summary_MRCOuta_CJ.RData"))

require(ggplot2)
require(dplyr)
require(tidyr)

### pools
df <- log_Summary %>%
    filter(variable %in% c("ABio",  "BBio", "TotalDOM")) %>%
    mutate(group = paste(simID, areaName, treatment,
                         initComm, landtype, replicate, variable))

nSims <- length(unique(df$replicate))
    
dfPercentiles <- df %>%
    group_by(areaName, treatment,
             initComm, landtype, Time, variable) %>%
    summarise(p25 = quantile(value, 0.25),
              p50 = quantile(value, 0.5),
              p75 = quantile(value, 0.75),
              maxVal = max(value),
              minVal = min(value))
    


png(filename= paste0("pools_Summary_singleCellSims.png"),
    width = 8, height = 5, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = Time, y = value*unitConvFact,
               colour = variable, fill = variable)) +
              #linetype = tenure, +
    theme_dark() +
    facet_grid(treatment ~ landtype) +
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
         subtitle = paste0("Medians are represented by coloured lines",
                           "\nEntire value ranges are represented by lightly Shaded areas (", nSims, " replicates)",
                           "\n25th and 75th percentiles are represented by the darker shaded areas.",
                           "\nA random simulation is represented in black"),
             x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")),
         caption = paste0("ABio : Aboveground biomass stocks.",
                          "\nBBio : Belowground (root) biomass stocks.",
                          "\nTotalDOM : Total dead organic matter and soil stocks."))

dev.off()

