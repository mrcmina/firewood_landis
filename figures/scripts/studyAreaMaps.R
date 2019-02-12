rm(list = ls())
setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(ggmap)
require(rgdal)
require(ggplot2)
require(OpenStreetMap)
require(mapproj)


studyArea <- readOGR(dsn = "../gis", layer = "MRC")
studyAreaLL <- spTransform(studyArea, CRS("+init=epsg:4326"))
studyAreaF <- fortify(studyArea)


############################################################
############################################################
#########
#########   Study area map
#########
############################################################
############################################################
bb <- bbox(studyAreaLL)
bb[,1] <- bb[,1]-1
bb[,2] <- bb[,2]+1

fond <- get_map(location = bb,  maptype = "satellite", source = "google")

map <- ggmap(fond) +
    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'white', fill = 'red', alpha = .1, size = .3) +
    labs(x = "longitude",
         y = "latitude")

png(filename = "studyArea.png",
    width = 2000, height = 1500, units = "px", res = 300, pointsize = 12,
    bg = "white")

print(map + theme_bw() +
          coord_map(projection = "mercator", 
                    xlim=c(attr(fond, "bb")$ll.lon, attr(fond, "bb")$ur.lon),
                    ylim=c(attr(fond, "bb")$ll.lat, attr(fond, "bb")$ur.lat)) +
          #theme_nothing() +
          theme(plot.title = element_text(size = rel(1.2)),
                #axis.title = element_text(size = rel(1)),
                axis.text = element_text(size = rel(0.5)),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_line(size = 0.1))
)

dev.off()

