###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha


#####
source("../scripts/fetchHarvestImplementationFnc.R")

### fetching outputs
simDir <- "D:/Landis_ForCS_test"
simInfo <- read.csv(paste(simDir, "simInfo.csv", sep = "/"),
                    colClasses=c("simID"="character"))
mgmtAreas_RAT <- read.csv(paste(simDir, "mgmtAreas_RAT.csv", sep = "/"))
###################################################################
prescriptLvls <- c("CPRS", "CJ", "Firewood")



require(data.table)
require(dplyr)
require(raster)
require(doSNOW)
require(parallel)
require(foreach)

clusterN <- 4
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)


outputSummaryLandscape <- foreach(i = 1:nrow(simInfo), .combine="rbind") %dopar% {
    require(dplyr)
    require(raster)
    require(reshape2)
    require(data.table)
    require(dplyr)
    require(raster)

    mgmt_RAT <- filter(mgmtAreas_RAT, area == simInfo[i, "areaName"])
    
    sDir <- paste(simDir, simInfo[i,"simID"], sep ="/")
    
    ### fetching management areas
    mgmtAreas <- raster(paste(sDir, "management-areas.tif", sep = "/"))
    mgmtAreasVals <- unique(values(mgmtAreas))
    mgmtAreasVals <- mgmtAreasVals[!is.na(mgmtAreasVals)]
    mgmtAreasVals <- mgmtAreasVals[order(mgmtAreasVals)]
    
    ### computing area size (ha)
    # first, reclassify 'pas de récolte' to 'privé'
    index <- grep("Pas de récolte", mgmt_RAT$value)
    for (id in index) {
        m <-  gsub(" - Pas de récolte", " - Privé", mgmt_RAT[id, "value"])
        newVal <- mgmt_RAT[match(m, mgmt_RAT$value), "id"]
        mgmtAreas[mgmtAreas == id] <- newVal
    }
    mgmtAreaSize <- data.frame(freq(mgmtAreas))
    mgmtAreaSize[,"area_ha"] <- mgmtAreaSize$count * (prod(res(mgmtAreas))/10000)
    mgmtAreaSize <- data.frame(mgmtAreaID = mgmtAreaSize$value,
                               area_ha = mgmtAreaSize$area_ha)
    mgmtAreaSize <- mgmtAreaSize[complete.cases(mgmtAreaSize),]
   
    
    ### fetching harvest implementation table
    x <- paste(sDir, "base-harvest.txt", sep = "/")
    harvImpl <- fetchHarvestImplementation(x)
    prescriptLvls <- unique(harvImpl$prescription)
    
    #############
    index <- which(!is.na(values(mgmtAreas)))
    mgmtVal <- values(mgmtAreas)[index]
    XY <- rowColFromCell(mgmtAreas, index)
    colnames(XY) <- c("row", "column")
    XY <- data.frame(XY,
                     mgmtAreaID = mgmtVal,
                     mgmtAreaName = mgmt_RAT[match(mgmtVal, mgmt_RAT$id), "value"])
    
    ####
    logSummary <- fread(file = paste(sDir, "log_Summary.csv", sep = "/"))#, nrows = 1000)
    
    ### subsetting outputs
    df <- merge(XY, logSummary)
    
    
    ### summarizing by management area
    dfSummary <- df %>%
        group_by(mgmtAreaName, mgmtAreaID, Time) %>%
        summarise(treatment = simInfo[i, "treatment"],
                  ABio = mean(ABio),
                  BBio = mean(BBio),
                  TotalDOM = mean(TotalDOM),
                  DelBio = mean(DelBio),
                  Turnover = mean(Turnover),
                  NetGrowth = mean(NetGrowth),
                  NPP = mean(NPP),
                  Rh = mean(Rh), 
                  NEP = mean(NEP),
                  NBP = mean(NBP)) %>%
        merge(mgmtAreaSize)
        
    
    
    dfSummary <- melt(dfSummary, id.vars = c("mgmtAreaName", "mgmtAreaID", "Time", "area_ha", "treatment"))
    dfSummary <- data.frame(MRC = gsub(" - Pas de récolte| - Privé| - Public", "", dfSummary$mgmtAreaName),
                            dfSummary)
    dfSummary <- data.frame(tenure = gsub(paste(paste(unique(dfSummary$MRC), "- "), collapse = "|"), "",
                                          dfSummary$mgmtAreaName),
                            dfSummary)

    return(dfSummary)
    
}

stopCluster(cl)
save(outputSummaryLandscape, file = "outputSummaryLandscape.RData")





