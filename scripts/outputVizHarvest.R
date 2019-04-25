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


### visualizing harvesting

clusterN <- 2
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

outputSummaryLandscape <- foreach(i = 1:nrow(simInfo), .combine="rbind") %dopar% {
    require(colorspace)
    require(stringr)
    require(ggplot2)
    require(tidyr)
    require(data.table)
    require(dplyr)
    require(raster)
    require(doSNOW)
    require(parallel)
    require(foreach)

    areaName <- as.character(simInfo[i, "areaName"])
    simID <- simInfo[i, "simID"]
    ### harvesting
    sDir <- paste(simDir, simInfo[i, "simID"], sep ="/")

    ### loading mgmtAreas, cleaning up, and reformatting 
    mgmtAreas <- raster(paste(sDir, "management-areas.tif", sep = "/"))
    initComm <- raster(paste(sDir, "initial-communities.tif", sep = "/"))
    mgmtAreas[is.na(initComm)] <- NA
    mgmt_RAT <- filter(mgmtAreas_RAT, area == areaName)
    MRC_names <- unique(gsub(" - Privé| - Public| - Pas de récolte", "", mgmt_RAT$value))
    treat <- as.character(simInfo[i, "treatment"])
    MRC_Poly <- get(load(paste0("../data/studyAreaP_", areaName, ".RData")))
    MRC_F <- fortify(MRC_Poly)
    
    ### fetching harvest implementation table
    x <- paste(sDir, "base-harvest.txt", sep = "/")
    harvImpl <- fetchHarvestImplementation(x)
    prescriptLvls <- unique(harvImpl$prescription)
    prescript_RAT <- data.frame(prescriptName = prescriptLvls,
                                id = as.numeric(prescriptLvls)+1,
                                color = c("darkolivegreen3", "darkturquoise", "red")[1:length(prescriptLvls)])

    ### fetching harvest outputs and reformatting 
    x <- list.files(paste(sDir, "harvest", sep = "/"))
    x <- x[grep(".img", x)]
    simYear <- as.numeric(gsub("[^0-9]", "", x))
    x <- x[order(simYear)]
    simYear <- simYear[order(simYear)]

    harv <- stack(paste(sDir, "harvest", x, sep = "/"))
    ## setting CRS
    crs(harv) <- crs(mgmtAreas)
    #res(harv) <- res(mgmtAreas)
    extent(harv) <-  extent(mgmtAreas)

    harv[is.na(mgmtAreas)|harv == 0] <- NA
    ### compute last prescription applied
    foreach(l = 1:100) %do% {
        r <- harv[[l]]
        if (l==1) {
            harvLastPrescript <- r
        } else {
            harvLastPrescript[r>1] <- r[r>1]
        }
        print(l)
    }
    
    
    
    
    ### computing zonal statistics
    
    hVal <- values(harv)
    hVal[hVal == 1] <- NA

    mgmVal <- values(mgmtAreas)
    mgmNCells <- as.numeric(table(mgmVal))
    ts <- as.numeric(gsub("[^0-9]", "", colnames(hVal))) 
    timestep <- unique(diff(ts))
    harvRates <- foreach(l = 1:ncol(hVal), .combine = "rbind") %do% {
        hN <- t(table(hVal[,l], mgmVal))
        hRate <- apply(hN, 2, function(x) x/mgmNCells)
        colnames(hRate) <- prescript_RAT[match(as.numeric(colnames(hRate)), prescript_RAT$id), "prescriptName"]
        hRate <- data.frame(Time = as.numeric(gsub("prescripts.", "", colnames(hVal)[l])),
                            mgmtArea = as.numeric(rownames(hRate)),
                            hRate)
        if ("Firewood" %in% prescriptLvls) {
            hRate <- gather(hRate,
                            key = prescription, value = harvestAreaProp,
                            CPRS, CJ, Firewood)  
        } else {
            hRate <- gather(hRate,
                            key = prescription, value = harvestAreaProp,
                            CPRS, CJ)
        }

        ### converting to annual rate
        hRate$harvestAreaProp <- hRate$harvestAreaProp/timestep
        
        ##############################
        return(hRate)
    }
    harvRates <- harvRates %>%
        merge(harvImpl) 
    harvRates[,"mgmtArea_Name"] <- mgmt_RAT[match(harvRates$mgmtArea, mgmt_RAT$id), "value"]
    pWidth <- 2.5*length(prescriptLvls)+0.5
    
    ### plotting harv. rates
    p <- ggplot(harvRates, aes(x = Time, y = harvestAreaProp * 100)) +
        
        theme_dark() +
        geom_line(colour = "yellow", size = 1) +
        geom_hline(data = distinct(harvRates[,c("mgmtArea_Name","prescription", "harvestAreaProp_target")]),
                   aes(yintercept = harvestAreaProp_target * 100),
                   linetype = 3, size = 0.75, colour = "cyan") +
        
        facet_grid(mgmtArea_Name ~ prescription)
    
    
        # scale_fill_manual("",
        #                   values = c(colorRampPalette(c("darkblue", "black"))(100)[50],
        #                              as.character(prescript_RAT$color)),
        #                   labels = c("no harvest", as.character(prescript_RAT$prescriptName))) +
        # geom_polygon(aes(x = long, y = lat, group = group), data = MRC_F,
        #              colour = 'white', fill = NA, size = 0.5)
        # 
    png(filename = paste0("harvRates_", areaName,"_", simID, ".png"),
        width = pWidth, height = 8, units = "in", res = 300, pointsize = 10,
        bg = "white")
    
    print(p +
              theme(#legend.position="top", legend.direction="horizontal",
                  #axis.text =  element_text(size = rel(0.35)),
                  #axis.text.y = element_text(angle = 90, hjust = 0.5),
                  legend.title = element_text(size = rel(0.85)),
                  title = element_text(size = rel(0.85)),
                  #plot.subtitle = element_text(size = rel(1)),
                  plot.caption = element_text(size = rel(0.65))) +
              labs(title = "Harvesting rates",
                   subtitle = paste("simID:", simInfo[i, "simID"],
                                    "; areaName:", simInfo[i, "areaName"],
                                    "; treatment:", simInfo[i, "treatment"]),
                   x = "Time",
                   y = "Harvest rates\n(% of mgmt units)")
    )
    dev.off()
    
    
    ### plotting individual time steps

    files <- character()
    for  (y in seq_along(simYear)) {
        year <- simYear[y]
        r <- harv[[y]]
        df <- data.frame(rasterToPoints(r))
        colnames(df)[3] <- "id"
        df[,"value"] <- prescript_RAT[match(df$id, as.factor(prescript_RAT$id)), "prescriptName"]


        fTitle <- paste0("harv_",
                         areaName,
                         "_",
                         treat,
                         "_",
                         str_pad(year, nchar(max(simYear)), pad = "0"), ".png")

        p <- ggplot(df, aes(x = x, y = y, fill = as.factor(id))) +
            geom_raster() +
            theme_dark() +
            coord_equal() +
            scale_fill_manual("",
                              values = c(colorRampPalette(c("darkblue", "black"))(100)[50],
                                         as.character(prescript_RAT$color)),
                              labels = c("no harvest", as.character(prescript_RAT$prescriptName))) +
            geom_polygon(aes(x = long, y = lat, group = group), data = MRC_F,
                         colour = 'white', fill = NA, size = 0.5)

        png(filename = fTitle,
            width = 8, height = 5, units = "in", res = 300, pointsize = 10,
            bg = "white")

            print(p +
                      theme(#legend.position="top", legend.direction="horizontal",
                          axis.text =  element_text(size = rel(0.35)),
                          axis.text.y = element_text(angle = 90, hjust = 0.5),
                          legend.title = element_text(size = rel(0.85)),
                          title = element_text(size = rel(0.85)),
                          #plot.subtitle = element_text(size = rel(1)),
                          plot.caption = element_text(size = rel(0.65))) +
                      labs(title = "Harvesting",
                          subtitle = paste0(paste(MRC_names, collapse = " - "), "\n", "Year : ", year),
                          x = "",
                          y = "")
            )
        dev.off()
        files <- append(files, fTitle)
    }
    
    ### summary figures
    r <- harvLastPrescript
    df <- data.frame(rasterToPoints(r))
    colnames(df)[3] <- "id"
    df[,"value"] <- prescript_RAT[match(df$id, as.factor(prescript_RAT$id)), "prescriptName"]
    
    p <- ggplot(df, aes(x = x, y = y, fill = as.factor(id))) +
        geom_raster() +
        theme_dark() +
        coord_equal() +
        scale_fill_manual("",
                          values = c(colorRampPalette(c("darkblue", "black"))(100)[50],
                                     as.character(prescript_RAT$color)),
                          labels = c("no harvest", as.character(prescript_RAT$prescriptName))) +
        geom_polygon(aes(x = long, y = lat, group = group), data = MRC_F,
                     colour = 'white', fill = NA, size = 0.5)
    
    fTitleLast <- paste0(gsub("[0-9]|.png", "", fTitle), "lastPrescr.png")
    fTitleAnim <- gsub("lastPrescr.png", "anim.gif", fTitleLast)
    png(filename = fTitleLast,
        width = 8, height = 5, units = "in", res = 300, pointsize = 10,
        bg = "white")
    
    print(p +
              theme(#legend.position="top", legend.direction="horizontal",
                  axis.text =  element_text(size = rel(0.35)),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  legend.title = element_text(size = rel(0.85)),
                  title = element_text(size = rel(0.85)),
                  #plot.subtitle = element_text(size = rel(1)),
                  plot.caption = element_text(size = rel(0.65))) +
              labs(title = "Harvesting",
                   subtitle = paste0(paste(MRC_names, collapse = " - "), "\n",
                                     "Last precription applied after ",max(simYear), " years of simulation"),
                   x = "",
                   y = "")
    )
    dev.off()
    
    file.copy(fTitleLast, "tmp.png")
    require(animation)
    oopt = ani.options(ani.dev="png", ani.type="png",
                       interval = 0.3, autobrowse = FALSE)


    im.convert(c(files, rep("tmp.png", 20)), output = fTitleAnim,
               extra.opts = "", clean = T)
    
}
