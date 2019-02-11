rm(list=ls())
require(raster)
require(dplyr)
require(RCurl)
require(tidyr)
require(rgdal)
require(ggplot2)

setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
rm(wwd)
inputFolder <- "../initAGB_Beaudoin2014"

#
areas <- "MRCOuta"
a <- areas[1]

## fetching polygons for study area
studyAreaP <- get(load(paste0("../data/studyAreaP_", a, ".RData")))

#
simAreaName <- paste(unique(studyAreaP$MRS_NM_REG), collapse = " - ")

files <- list.files(inputFolder)
files <- files[grep("initBiomassKnnTonsPerHa", files)]
#
fileInfo <- strsplit(gsub(".tif", "", files), "-")
fileArea <- as.character(lapply(fileInfo, function(x) x[2]))
fileSpp <- as.character(lapply(fileInfo, function(x) x[3]))
fileSpp <- gsub("-", ".", fileSpp)

areaIndex <- which(fileArea == a)
x <- files[areaIndex]
spp <- fileSpp[areaIndex]

### fetching AGB rasters from beaudoin et al. 
sppStack <- stack(paste(inputFolder, x, sep="/"))
### adding 'sum' layer
sppStack <- stack(sppStack, sum(sppStack, na.rm = T))
### naming layers
spp <- c(spp, "Total")
#

sppStack <- rasterToPoints(sppStack)
colnames(sppStack)[3:ncol(sppStack)] <- spp

sppStack <- as.data.frame(sppStack) %>% 
    gather(species, biomass_tonsPerHa, -x, -y)
sppStack$species <- factor(sppStack$species, levels = spp)
sppStack <- sppStack[complete.cases(sppStack),]


sppLabel <- sppStack %>%
    filter(biomass_tonsPerHa >=0.0001) %>%
    group_by(species) %>%
    summarize(meanBiomass_tonsPerHa = mean(biomass_tonsPerHa))


ncol <- 5
nrow <- ceiling(nrow(sppLabel)/ncol)
pWidth <- 10
pHeight <- (nrow)*2
zLim <- c(0, ceiling(quantile(sppStack$biomass_tonsPerHa, 0.9999)/10)*10)
ylim <- range(sppStack$y)
xlim <- range(sppStack$x)

## study area (optional)
studyAreaF <- fortify(studyAreaP)

#df <- filter(sppStack, species %in% spp[1:3])
### very long... 
p <- ggplot(data = sppStack, aes(x = x, y = y)) +
    geom_raster(aes(fill = biomass_tonsPerHa)) +
    facet_wrap(~ species, ncol = ncol) +
    scale_fill_gradientn(name = "AGB (tonnes/ha)",
                         colours = c("grey", "darkblue", "darkgreen", "yellow",  "orange", "red"),
                         values = c(0, 0.001, 0.20, 0.40, 0.60, 1), limits = zLim) +
    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'white', fill = NA, size = 0.6) +
    geom_text(aes(x = max(xlim), y =  max(ylim),
                  label = "mean AGB"),
              colour = "white",
              hjust = 1, vjust = 1, size = rel(3), fontface = 1) +
    geom_text(aes(x = x, y = y,
                  label = paste(signif(meanBiomass_tonsPerHa, 2), "tonnes/ha")),
              colour = "white",
              data = data.frame(sppLabel, x = max(xlim), y =  max(ylim)-(diff(ylim)/12.5)),
              hjust = 1, vjust = 1, size = rel(3), fontface = "bold") +
    coord_equal()


png(filename = paste0("initialBiomass_", a, ".png"),
    width = pWidth, height = pHeight, units = "in", res = 300, pointsize = 10)


print(p + labs(title = paste0("k-NN estimates of initial aboveground biomass in 2001\n",
                              simAreaName),
               caption = "k-NN estimates of AGB are from Beaudoin et al. 2014.") +
          theme_dark() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y =  element_blank(),
                #strip.background = element_blank(),
                strip.text.x = element_text(size = rel(0.8))))
dev.off()