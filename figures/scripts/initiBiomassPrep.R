rm(list = ls())

setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
sourceDir <- "D:/BiomassKnn"
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(RCurl)
require(raster)

areas <- "MRCCentre"
year <- 2001

a <- areas[1]

## fetching species
species <- read.table(paste0("../inputs/species_", a, ".txt"),
                      skip = 1, comment.char = ">")

## loading landtypes
landtypes <- raster(paste0("../inputs/landtypes_", a, ".tif"))
## proporiton of forest within pixel
forestProp <- raster(paste0(sourceDir, "/NFI_MODIS250m_",year, "_kNN_LandCover_VegTreed_v1.tif"))
forestProp <- resample(forestProp, landtypes, method = "ngb")
#forestProp <- crop(forestProp, landtypes)
forestProp[forestProp == 0] <- NA

ABGTotal <- raster(paste0(sourceDir, "/NFI_MODIS250m_", year, "_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif"))
ABGTotal <- resample(ABGTotal, landtypes, method = "ngb")
# plot(ABGTotal)

## listing source data files (from Beaudoin et al)
files  <- list.files(paste0(sourceDir))
files <-  files[grep(".tif", files)]
files <- files[-(grep(".aux.xml", files))]
# extracting info
fileInfo <- strsplit(files, "_")
yrInfo <- as.numeric(lapply(fileInfo, function(x) x[3]))
typeInfo <- as.character(lapply(fileInfo, function(x) x[5]))




## fetching area's species list
#spp <- as.character(vegCodes[vegCodes[, a] == 1, "LandisCode"] )
spp <- species[,1]


#
spFiles <- files[typeInfo == "Species" & yrInfo == year]

spInfo <- strsplit(spFiles, "_")
spInfo <- toupper(as.character(lapply(spInfo, function(x) paste(x[6], x[7], sep = "."))))




## cycling through species
for (i in seq_along(spp)) {
    sp <- as.character(spp[i])
    ##
    if (sp == "THUJ.SPP.ALL") {
        sp <- "THUJ.OCC"
    }
    
    r <- raster(paste(sourceDir, spFiles[spInfo == sp], sep ="/"))
    r <- resample(r, landtypes, method = "ngb")
    
    
    
    ### special cases where multiple species where summed up
    if (sp == "POPU.TRE") { ## combine with POPU.BAL and POPU.SPP
        r2 <- raster(paste(sourceDir, spFiles[spInfo == "POPU.BAL"], sep ="/"))
        r2 <- resample(r2, landtypes, method = "ngb")
        r3 <- raster(paste(sourceDir, spFiles[spInfo == "POPU.SPP"], sep ="/"))
        r3 <- resample(r3, landtypes, method = "ngb")
        r <- r + r2 + r3
    }
  
    r <- (r/100)*ABGTotal
    # # convert total tons to tons per ha 
    # r <- r / (prod(res(r))/10000)
    # # consider only productive forests
    # r <- r / (forestProp/100)
    fName <- paste0("initBiomassKnnTonsPerHa-", a, "-", sp, ".tif")
    writeRaster(r, filename = fName, overwrite = TRUE)
    print(paste(a, sp))
}


