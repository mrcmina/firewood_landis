###################################################################
###################################################################
### ForCS output processing for single cell simulations
rm(list = ls())
setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha

#####
source("../scripts/fetchHarvestImplementationFnc.R")

### fetching outputs
simDir <- "."
simInfo <- read.csv(paste(simDir, "simInfo.csv", sep = "/"),
                    colClasses=c("simID"="character"))

areaName <- unique(simInfo$areaName)
treatment <- unique(simInfo$treatment)

###################################################################
prescriptLvls <- c("CPRS", "CJ", "Firewood")

require(data.table)
require(dplyr)
require(doSNOW)
require(parallel)
require(foreach)

clusterN <- 6 ### seems optimal for 
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
t1 <- Sys.time()

################################################################################
### Biomass

log_BiomassC <- foreach(i = 1:nrow(simInfo), .combine="rbind") %dopar% {
    require(dplyr)

    simID <- simInfo[i,"simID"]
    sDir <- paste(simDir, simID, sep ="/")
    landtype <- simInfo[i,"landtypes"]
    initComm <- simInfo[i,"initComm"]
    replicate <- simInfo[i,"replicate"]
    
    x <- read.csv(paste(sDir, "log_BiomassC.csv", sep = "/"))
    x <- x %>%
        select(Time, species, Age, Wood, Leaf, CrsRoot, FineRoot)
    x <- data.frame(simID = simID,
                    areaName = areaName,
                    treatment = treatment,
                    initComm = initComm,
                    landtype = landtype,
                    replicate = replicate,
                    x)
    return(x)
}
log_BiomassC <- melt(log_BiomassC, id.vars = c("simID", "areaName", "treatment", "initComm",
                               "landtype", "replicate", "Time", "species", "Age"))
save(log_BiomassC, file = paste0("log_BiomassC_",areaName, "_", treatment, ".RData"))


################################################################################
### DOM & SOM
t1 <- Sys.time()

log_Pools <- foreach(i = 1:nrow(simInfo), .combine="rbind") %dopar% {
    require(dplyr)
    
    simID <- simInfo[i,"simID"]
    sDir <- paste(simDir, simID, sep ="/")
    landtype <- simInfo[i,"landtypes"]
    initComm <- simInfo[i,"initComm"]
    replicate <- simInfo[i,"replicate"]
    
    x <- read.csv(paste(sDir, "log_Pools.csv", sep = "/"))
    x <- x %>%
        select(Time, species, VF_A, VF_B, Fast_A,
               MED, Slow_A, Slow_B, Sng_Stem, Sng_Oth)
    
    x <- data.frame(simID = simID,
                    areaName = areaName,
                    treatment = treatment,
                    initComm = initComm,
                    landtype = landtype,
                    replicate = replicate,
                    x)
    return(x)
}
log_Pools <- melt(log_Pools, id.vars = c("simID", "areaName", "treatment", "initComm",
                                            "landtype", "replicate", "Time", "species"))
save(log_Pools, file = paste0("log_Pools_",areaName, "_", treatment, ".RData"))


################################################################################
### summary

log_Summary <- foreach(i = 1:nrow(simInfo), .combine="rbind") %dopar% {
    require(dplyr)
    
    simID <- simInfo[i,"simID"]
    sDir <- paste(simDir, simID, sep ="/")
    landtype <- simInfo[i,"landtypes"]
    initComm <- simInfo[i,"initComm"]
    replicate <- simInfo[i,"replicate"]
    
    x <- read.csv(paste(sDir, "log_Summary.csv", sep = "/"))
    x <- x %>%
        select(Time, ABio, BBio, TotalDOM,
               DelBio, Turnover, NetGrowth,	NPP, Rh, NEP, NBP)
    
    x <- data.frame(simID = simID,
                    areaName = areaName,
                    treatment = treatment,
                    initComm = initComm,
                    landtype = landtype,
                    replicate = replicate,
                    x)
    return(x)
}
log_Summary <- melt(log_Summary, id.vars = c("simID", "areaName", "treatment", "initComm",
                                         "landtype", "replicate", "Time"))
save(log_Summary, file = paste0("log_Summary_",areaName, "_", treatment, ".RData"))
