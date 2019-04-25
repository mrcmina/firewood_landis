### A function that reads the LANDIS-II 'base-harvest.txt' input file
### and returns the harvest implementation table as a data.frame

fetchHarvestImplementation <- function(x, prescripLvls) {
    
    HarvestImpTmp <- readLines(x)
    index <- grep("HarvestImplementations|PrescriptionMaps", HarvestImpTmp)
    HarvestImpTmp <- HarvestImpTmp[index[1]:(index[2]-1)]
    HarvestImpTmp <-  HarvestImpTmp[grep(paste(prescriptLvls, collapse = "|"), HarvestImpTmp)]
    #### putting into a nice data frame
    for (i in seq_along(HarvestImpTmp)) {
        x <- strsplit(HarvestImpTmp[[i]], "\\t")[[1]]
        x <- x[nchar(x)!=0]
        tmp <- data.frame(mgmtArea = as.numeric(x[1]),
                          prescription = factor(x[2], levels = prescriptLvls),
                          harvestAreaProp_target = as.numeric(gsub("%", "", x[3]))/100,
                          beginTime = x[4])
        if(i == 1) {
            HarvestImplementations <- tmp
        } else {
            HarvestImplementations <- rbind(HarvestImplementations, tmp)
        }
    }
    return(HarvestImplementations)
}

