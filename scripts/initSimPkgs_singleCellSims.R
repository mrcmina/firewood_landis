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
require(raster)

#spinUp <- T

inputDir <- "../inputs"
timestep <- 1
simDuration <- 500
simArea <- "MRCOuta"
#inputs <- list.files(inputDir)



### loading landtypes
landtypes <- raster(paste0(inputDir, "/landtypes_", simArea, ".tif"))
# table(values(landtypes))

##############################

speciesList <- list(mishmash = paste(c("ACER.SAH 1", "BETU.ALL 1",
                                       "FAGU.GRA 1", "POPU.TRE 1",
                                       "QUER.RUB 1", "ABIE.BAL 1",
                                       "PICE.GLA 1", "TSUG.CAN 1"), collapse = "\n"))

timestep <- 1
expDesign <- list(area = simArea,
                  landtypes = c("423_1", "425_1"),
                  treatment = list("CPRS" = seq(from = 100, to = simDuration, by = 35)),
                  nrep = 25)

simInfo <- expand.grid(areaName = expDesign$area,
                       landtypes = expDesign$landtypes,
                       treatment = names(expDesign$treatment),
                       initComm = names(speciesList),
                       replicate = 1:expDesign$nrep)

sID <- (1:nrow(simInfo))-1
simInfo <- data.frame(simID = str_pad(sID, nchar(max(sID)),
                                      pad = "0"),
                      simInfo)

# require(parallel)
# require(doSNOW)
# n <- floor(detectCores() * .25)
# 
# # #######
# cl = makeCluster(n, outfile = "") ## 
# registerDoSNOW(cl)
#foreach(i = 1:nrow(simInfo)) %dopar% {

for (i in 1:nrow(simInfo)) {
    require(stringr)
    require(raster)
    
    dt <- "INT4S" ### raster encoding type
    
    landtypesTableFile <- paste0(inputDir, "/landtypes_", simArea, ".txt")
    landtypes_RAT <- read.table(landtypesTableFile, skip = 1,comment.char = ">")
    colnames(landtypes_RAT) <- c("Active", "Code", "Name", "Description")
    
    landtypeName <- as.character(simInfo[i, "landtypes"])
    landtypeCode <- landtypes_RAT[match(landtypeName, landtypes_RAT$Name), "Code"]
    
    simID <- as.character(simInfo[i,"simID"])
    areaName <- as.character(simInfo[i,"areaName"])
    replicate <- as.character(simInfo[i,"replicate"])
    initComm <- speciesList[[as.character(simInfo[i,"initComm"])]]
    
    prescript <- expDesign$treatment[simInfo[i, "treatment"]]
    
    
    dir.create(simID)
    
    ### creating raster files and associated attribute tables
    InitCommVal <- 1
    r <- raster(matrix(InitCommVal))
    
    rLandtype <- r
    rLandtype[] <- landtypeCode
    
    writeRaster(rLandtype,
                filename = paste(simID, "landtypes.tif", sep = "/"),
                overwrite = T, datatype = dt)
    writeRaster(r,
                filename = paste(simID, "initial-communities.tif", sep = "/"),
                overwrite = T, datatype = dt)
    
    file.copy(landtypesTableFile, paste(simID, "landtypes.txt", sep ="/"), overwrite = T)
    sink(file = paste(simID, "initial-communities.dat", sep = "/"))
    
        cat('LandisData "Initial Communities"\n\n')
        cat(paste("MapCode", InitCommVal, "\n"))
        cat(initComm)
        cat("\n")
        
    sink()
    
    
    
    ##############################################
    ### Succession extension
    
    # ForC-succession
    x <- paste0(paste0(inputDir, "/ForC-succession_",
                       areaName, ".txt"))
    
    x <- readLines(x)
    index <- grep("ForCSOutput|SoilSpinUp", x)
    
    fName <-  paste0(simID, "/ForC-succession.txt")
    
    sink(file = fName)
    
    cat(paste(x[1:index[1]], collapse = "\n"))
    cat("\n")
    cat(">> How Frequently the four different output files should be printed.  (intervals in years)\n")
    cat(">> Output interval\n")
    cat(">>  Biomass\tDOM_Pools\tFluxes\tSummary\n")
    cat(">>  ----------------------------------------------------------------\n")
    
    cat(paste(c(rep(timestep, 4), "\n\n"), collapse = "\t"))
    
    cat(paste(x[index[2]:length(x)], collapse = "\n"))
    sink()
    
    # file.copy(paste0(inputDir, "/ForC-succession_",
    #                  areaName, ".txt"),
    #           paste0(simID, "/ForC-succession.txt"),
    #           overwrite = T)
    
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
    writeRaster(r, paste0(simID, "/stand-map.tif"),
              overwrite = T, datatype = dt)
    
    # management areas
    writeRaster(r, paste0(simID, "/management-areas.tif"),
                overwrite = T, datatype = dt)
    
    
    ###############################################
    ### base-harvest.txt
    x <- paste0(inputDir, "/base-harvest_",
                areaName, "_firewoodSinglePass_", timestep, "yrTS.txt")
    
    x <- readLines(x)
    index <- grep("HarvestImplementations|PrescriptionMaps", x)
    
    

    
    harvImpl <- data.frame(MgmtArea = values(r),
                           Prescription = names(prescript),
                           HarvestArea = "100%",
                           BeginTime = prescript[[1]],
                           EndTime = prescript[[1]])
    
    fName <-  paste(simID, "base-harvest.txt", sep = "/")
    sink(file = fName)
    
    cat(paste(x[1:index[1]], collapse = "\n"))
    
    cat(paste("\n>>", paste(colnames(harvImpl), collapse = "\t"), "\n\n"))
    
    sink()
    write.table(harvImpl, file  = fName, append = T,
                col.names = F, row.names = F, quote = F)
    
    sink(file = fName, append = T)
    
    cat(paste(x[index[2]:length(x)], collapse = "\n"))
    
    sink()
    
    
    

    ###############################################
    ### scenario.txt
    
    x <- paste0(inputDir, "/scenario.txt")
    
    x <- readLines(x)
    # duration
    index <- grep("Duration", x)
    x[3] <- paste("Duration", simDuration)

    sink(file = paste(simID, "scenario.txt", sep = "/"))
    cat(paste(x, collapse = "\n"))
    sink()
    
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
file.copy("../scripts/simPilot_singleCellSims.R",
          "simPilot.R",
          overwrite = T)
