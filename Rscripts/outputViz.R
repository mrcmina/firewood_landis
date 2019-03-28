
###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha
summaryLanscape <- get(load("./summaryLanscape.RData"))


require(ggplot2)


### pools
x <- summaryLanscape %>%
    filter(tenure = variable %in% c("ABio",  "BBio", "TotalDOM"))

png(filename= paste0("poolSummary.png"),
    width = 6, height = 6, units = "in", res = 600, pointsize=10)

ggplot(x, aes(x = Time, y = value*unitConvFact, group = mgmtAreaName, colour = tenure)) +
    facet_grid(variable ~ MRC) +
    geom_line() +
    labs(title = "Summary of aggregated pools",
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")))

dev.off()

# Disturbance type ID 1=fire, 2=harvest, 3=wind, 4=bda