#rm(list =ls())
# setwd("C:/Users/cyrdo/Sync/Travail/ECCC/Firewood/UQO/Landis-II/2019-04-17")
wwd <- getwd()

simInfo <- read.csv("simInfo.csv", colClasses = c(simID = "character"))
simDir <- simInfo$simID

require(parallel)
require(doSNOW)
n <- 4
# #######
cl = makeCluster(n, outfile = "") ## 
registerDoSNOW(cl)
t1 <- Sys.time()
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
    x <- list.files("harvest", full.names = T)
    x <- x[grep(".img", x)]
    unlink(x, force = T)
    
}
t2 <- Sys.time()
simD <- t2-t1
print(paste("Total time: ", simD))
print(paste(simD / length(simDir), "by simulation (", n, "cores )"))
stopCluster(cl)

### [1] "0.0668760623137156 by simulation ( 8 cores )"
# 
# [1] "Total time:  16.3784577528636"
# [1] "0.163784577528636 by simulation ( 32 cores )"

# [1] "Total time:  about 7 min"
# [1] "0.07 by simulation ( 16 cores )"
