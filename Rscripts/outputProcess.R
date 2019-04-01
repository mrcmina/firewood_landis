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
source("../Rscripts/fetchHarvestImplementationFnc.R")

### fetching outputs
simDir <- "D:/Landis_ForCS"
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

clusterN <- 6
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
    mgmtAreas <- raster(paste(sDir, "Inputs/input_rasters/managementAreas.tif", sep = "/"))
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
    x <- paste(sDir, "Inputs/base_harvest.txt", sep = "/")
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




# ### visualizing harvesting
# 
# clusterN <- 6
# #######
# cl = makeCluster(clusterN, outfile = "") ##
# registerDoSNOW(cl)
# 
# outputSummaryLandscape <- foreach(i = 1:nrow(simInfo), .combine="rbind") %dopar% {
#     require(colorspace)
#     require(stringr)
#     
#     ### harvesting
#     sDir <- paste(simDir, simInfo[i, "simID"], sep ="/")
#     
#     mgmtAreas <- raster(paste(sDir, "Inputs/input_rasters/managementAreas.tif", sep = "/"))
#     initComm <- raster(paste(sDir, "Inputs/input_rasters/initial_communities.tif", sep = "/"))
#     mgmt_RAT <- filter(mgmtAreas_RAT, area == simInfo[i, "areaName"])
#     MRC_names <- unique(gsub(" - Privé| - Public| - Pas de récolte", "", mgmt_RAT$value))
#     simArea <-  as.character(simInfo[i, "areaName"])
#     treat <- as.character(simInfo[i, "treatment"])
#     MRC_Poly <- get(load(paste0("../data/studyAreaP_", simArea, ".RData")))
#     MRC_F <- fortify(MRC_Poly)
#     ### fetching harvest implementation table
#     x <- paste(sDir, "Inputs/base_harvest.txt", sep = "/")
#     harvImpl <- fetchHarvestImplementation(x)
#     prescriptLvls <- unique(harvImpl$prescription)
#     prescript_RAT <- data.frame(prescriptName = prescriptLvls,
#                                 id = as.numeric(prescriptLvls)+1,
#                                 color = c("darkolivegreen3", "darkturquoise", "red"))
#     
#     sDir <- paste(simDir, simInfo[i,"simID"], sep ="/")
#     
#     x <- list.files(paste(sDir, "harvest", sep = "/"))
#     x <- x[grep(".img", x)]
#     simYear <- as.numeric(gsub("[^0-9]", "", x))
#     x <- x[order(simYear)]
#     simYear <- simYear[order(simYear)]
#     
#     harv <- stack(paste(sDir, "harvest", x, sep = "/"))
#     ## setting CRS
#     crs(harv) <- crs(mgmtAreas)
#     #res(harv) <- res(mgmtAreas)
#     extent(harv) <-  extent(mgmtAreas)
#     
#     harv[is.na(mgmtAreas)|harv == 0] <- NA
#     
#     files <- character()
#     for  (y in seq_along(simYear)) {
#         year <- simYear[y]
#         r <- harv[[y]]
#         df <- data.frame(rasterToPoints(r))
#         colnames(df)[3] <- "id"
#         df[,"value"] <- prescript_RAT[match(df$id, as.factor(prescript_RAT$id)), "prescriptName"]
#         
#         
#         fTitle <- paste0("harv_",
#                          simArea,
#                          "_",
#                          treat,
#                          "_",
#                          str_pad(year, nchar(max(simYear)), pad = "0"), ".png")
#         
#         p <- ggplot(df, aes(x = x, y = y, fill = as.factor(id))) +
#             geom_raster() +
#             theme_dark() +
#             coord_equal() +
#             scale_fill_manual("",
#                               values = c(colorRampPalette(c("darkblue", "black"))(100)[50],
#                                          as.character(prescript_RAT$color)),
#                               labels = c("no harvest", as.character(prescript_RAT$prescriptName))) +
#             geom_polygon(aes(x = long, y = lat, group = group), data = MRC_F,
#                          colour = 'white', fill = NA, size = 0.5)
#         
#         png(filename = fTitle,
#             width = 8, height = 5, units = "in", res = 300, pointsize = 10,
#             bg = "white")
#         
#             print(p +
#                       theme(#legend.position="top", legend.direction="horizontal",
#                           axis.text =  element_text(size = rel(0.35)),
#                           axis.text.y = element_text(angle = 90, hjust = 0.5),
#                           legend.title = element_text(size = rel(0.85)),
#                           title = element_text(size = rel(0.85)),
#                           #plot.subtitle = element_text(size = rel(1)),
#                           plot.caption = element_text(size = rel(0.65))) +
#                       labs(title = "Harvesting",
#                           subtitle = paste0(paste(MRC_names, collapse = " - "), "\n", "Year : ", year),
#                           x = "",
#                           y = "")
#             )
#         dev.off()
#         files <- append(files, fTitle)
#     }
# }

