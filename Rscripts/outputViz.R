
###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha
outputSummaryLandscape <- get(load("./outputSummaryLandscape.RData"))


require(ggplot2)


### pools
df <- outputSummaryLandscape %>%
    filter(tenure == "Privé",
           variable %in% c("ABio",  "BBio", "TotalDOM")) %>%
    mutate(group = paste(mgmtAreaName, treatment))


png(filename= paste0("poolSummary.png"),
    width = 8, height = 8, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = Time, y = value*unitConvFact, group = group,
              #linetype = tenure,
              colour = treatment)) +
    facet_grid(MRC ~ variable  ) +
    geom_line() +
    labs(title = "Summary of aggregated pools",
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")))

dev.off()

# Disturbance type ID 1=fire, 2=harvest, 3=wind, 4=bda