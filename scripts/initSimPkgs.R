################################################################################
################################################################################
### Generates simulation packages for LANDIS-II 
### Dominic Cyr, in collaboration with Yan Boulanger, Marco Mina, et Frédérik Doyon
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Firewood/UQO/Landis-II", sep ="/"))
################################################################################
################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
#############
require(stringr)

spinUp <- T

inputDir <- "../inputs"
#inputs <- list.files(inputDir)
    
timestep <- 1
expDesign <- list(scenario = "baseline",
                  area = c("MRCCentre", "MRCOuta"),
                  treatment = c("firewoodSinglePass", "noFirewood"),
                  nrep = 1)

simInfo <- expand.grid(scenario = expDesign$scenario, 
                       areaName = expDesign$area,
                       treatment = expDesign$treatment,
                       replicate = 1:expDesign$nrep)

sID <- (1:nrow(simInfo))-1
simInfo <- data.frame(simID = str_pad(sID, nchar(max(sID)),
                                      pad = "0"),
                      simInfo)

for (i in 1:nrow(simInfo)) {
    
    simID <- as.character(simInfo[i,"simID"])
    areaName <- as.character(simInfo[i,"areaName"])
    treatment <- as.character(simInfo[i,"treatment"])
    replicate <- as.character(simInfo[i,"replicate"])
    
    dir.create(simID)
    
    ###############################################
    ### initial rasters and attribute files
    
    # initial communities
    file.copy(paste0(inputDir, "/initial-communities_",
                    areaName, ".dat"),
              paste0(simID, "/initial-communities.dat"),
              overwrite = T)
    file.copy(paste0(inputDir, "/initial-communities_",
                     areaName, ".tif"),
              paste0(simID, "/initial-communities.tif"),
              overwrite = T)
    
    # landtypes
    file.copy(paste0(inputDir, "/landtypes_",
                     areaName, ".txt"),
              paste0(simID, "/landtypes.txt"),
              overwrite = T)
    file.copy(paste0(inputDir, "/landtypes_",
                     areaName, ".tif"),
              paste0(simID, "/landtypes.tif"),
              overwrite = T)

   ###############################################
    ### Succession extension
    
    # ForC-succession
    file.copy(paste0(inputDir, "/ForC-succession_",
                     areaName, ".txt"),
              paste0(simID, "/ForC-succession.txt"),
              overwrite = T)
    
    # Climate inputs
    file.copy(paste0(inputDir, "/climate-inputs_",
                     areaName, ".txt"),
              paste0(simID, "/climate-inputs.txt"),
              overwrite = T)
    # Biomass succession
    
    # other ?
    
    ###############################################
    ### Disturbances
    
    ### Harvesting
    # stand map
    file.copy(paste0(inputDir, "/stand-map_",
                     areaName, ".tif"),
              paste0(simID, "/stand-map.tif"),
              overwrite = T)
    # management areas
    file.copy(paste0(inputDir, "/management-areas_",
                     areaName, ".tif"),
              paste0(simID, "/management-areas.tif"),
              overwrite = T)
    
    # base-harvest.txt
    file.copy(paste0(inputDir, "/base-harvest_",
                     areaName, "_", treatment, "_", timestep, "yrTS.txt"),
              paste0(simID, "/base-harvest.txt"),
              overwrite = T)

    ###############################################
    ### scenario.txt
    file.copy(paste0(inputDir, "/scenario.txt"),
              simID,
              overwrite = T)
    ### species.txt
    file.copy(paste0(inputDir, "/species_",
                    areaName, ".txt"),
              paste0(simID, "/species.txt"),
              overwrite = T)

    write.table(t(simInfo[i,]), file = paste0(simID, "/README.txt"),
                quote = F, col.names = F)
    
}

write.csv(simInfo, file = "simInfo.csv", row.names = F)
### simPilot.R
file.copy("../scripts/simPilot.R",
          "simPilot.R",
          overwrite = T)
file.copy("../data/mgmtAreas_RAT.csv",
          "mgmtAreas_RAT.csv",
          overwrite = T)
