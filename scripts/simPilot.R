#rm(list =ls())
# wwd <- setwd("D:/Landis_ForCS_test")
wwd <- getwd()

simInfo <- read.csv("simInfo.csv", colClasses = c(simID = "character"))
simDir <- simInfo$simID

require(parallel)
require(doSNOW)
n <- 1 #floor(detectCores() * 0.90)

# #######
cl = makeCluster(n, outfile = "") ## 
registerDoSNOW(cl)

foreach(i = 1:length(simDir)) %dopar% { # length(simDir)
    # if (i <= n) {  ### to reduce the probability of several processes
    #     ### trying to access the same file at the same time (if necessary)
    #     Sys.sleep(runif(1)*2)
    # }
    
    setwd(paste(wwd, simDir[i], sep ="/"))
    sink(file = "README.txt", append = T)
    
    x <- as.character(shell("landis-extensions.cmd list", intern = T))
    cat("\n")
    cat("#######################################################################\n")
    cat("########### Installed LANDIS-II extensions\n")
    cat("#######################################################################\n")
    for (l in seq_along(x)) {
        cat(x[l])
        cat("\n")   
    }
    cat("\n")
    cat("#######################################################################\n")
    cat("########### System Info\n")
    cat(write.table(as.data.frame(Sys.info()),
                    quote = F, col.names = F))
    sink()
    
    shell("landis scenario.txt", wait = T)
}

stopCluster(cl)