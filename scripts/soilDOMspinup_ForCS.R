### PARAMETERIZATION INITIAL DOM & Soil pools for a simulation with LANDIS-ForCS ######
## Marco Mina - February 2019
##
## Procedure: after having run a simulation with soilDOM spinup activated (SoilSpinUp table. Col1=1)
##  1) Read the outputs of the size of the DOM and soil pools (log_Pools.csv)
##  2) Save values for year 0 into a dataframe 
##  3) Aggregate by ecoregion/species 
##  4) Check that all species are present and if not add minumum dummy values for those absent 
##  5) Save a table with pool values, decay rates and Q10 by ecoregion*species*DOMpool 
##
##  Values from this table need to be pasted in EcoSppDOMParam table in the "ForC Succession" input file
##  This avoid running the DOMsoil spinp for each simulation (SoilSpinUp table. Col1=0)
##
## Note: a table with ecoregion ID number (1,2,...) matching actual ecoregion code (ecoregion_code_[landscape].txt) is needed

library(tidyverse)
options(scipen=999)

setwd("C:/Users/marco/OneDrive - UQAM/Documents/Other Projects/FireWood-LANDIS/inputs_all/")
simpath <- "E:/LANDIS_ForCS/simulations/MRCCentre/Test01/outputs/Test02_T0-1_soilspinup/" #path with simulation output folder, if different from wd

log_Pools <- read.csv(paste0(simpath,"log_Pools.csv")) ##output file with soil and DOM Pools
ecocode <- read.table("ecoregion_code_MRCCentre.txt", header = TRUE) #table ecoregion based on order ecor file with their code

log_Pools0 <- log_Pools %>% filter(Time == 0) %>% dplyr::select(-X) #keep only Year 0
log_Pools0_eco <- as.data.frame(log_Pools0 %>%   group_by(ecoregion, species) %>% 
                                  summarise(`1`=mean(VF_A), `2`=mean(VF_B), `3`=mean(Fast_A), `4`=mean(Fast_B), `5`=mean(MED),`6`=mean(Slow_A), 
                                            `7`= mean(Slow_B), `8`= mean(Sng_Stem), `9`= mean(Sng_Oth), `10`= mean(Extra)) %>% 
                                  gather(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`, key = "DOMpool", value = "Amount") )
log_Pools0_eco <- merge(log_Pools0_eco, ecocode, by = "ecoregion", all.x = TRUE)
log_Pools0_eco <- log_Pools0_eco %>% dplyr::select(ecoregion_sim, species, DOMpool, Amount)
log_Pools0_eco$Amount <- round(log_Pools0_eco$Amount, 4)
log_Pools0_eco$Amount[log_Pools0_eco$Amount == 0] <- 0.0001 #Values cannot be 0. Thus, replace 0 with 0.0001 (mostly for Pool10)
subset(log_Pools0_eco, is.na(log_Pools0_eco$Amount)) #check for any NAs in Amount
log_Pools0_eco$Amount[is.na(log_Pools0_eco$Amount)] <- 0.0001 #should there be any NA, replace any NA with 0.0001
log_Pools0_eco$DOMpool <- as.numeric(log_Pools0_eco$DOMpool)

EcoSppDOMParam <- log_Pools0_eco[order( log_Pools0_eco$ecoregion_sim, log_Pools0_eco$species, log_Pools0_eco$DOMpool ),  ]

#Add sequence of Decay Rates (Kurz et al 2009) and Q10 values (dummy, only for climate change)
decayrates110 <- c(0.355, 0.5, 0.1435, 0.1435, 0.0374,0.015,0.0033,0.0187,0.0718,0.0001) 
q10values <- c(2.65,	2,	2,	2,	2,	2.65,	1,	2,	2,	2)

#In some ecoregions some species may be absent. Thus, use a loop by ecoregion to paste series of decay rates and q10
## and add missing species with dummy minimum value 0.001 (otherwise the pool cannot be initialized and the simulation crashes)
ecoregions <- unique(EcoSppDOMParam$ecoregion_sim)
speciesall <- unique(EcoSppDOMParam$species)
EcoSppDOMParam_new <- data.frame()

for(e in 1:length(ecoregions)) {
  eco <- ecoregions[e]
  aux <- filter(EcoSppDOMParam, ecoregion_sim == eco)
  specieseco <- unique(aux$species) #species present in ecoregion
  speciesadd <- setdiff(speciesall,specieseco) #species absent in ecoregion
  add <- length(rep(speciesadd, each=10))
  aux_add <- data.frame(ecoregion_sim=rep(eco,add), species=rep(speciesadd, each=10), DOMpool=rep(seq(1,10),add/10), Amount=rep(0.0001,add) )
  aux <- rbind(aux, aux_add)
  aux$DecayRate <- rep(decayrates110, times=length(unique(aux$ecoregion_sim))*length(unique(aux$species)))
  aux$Q10 <- rep(q10values, times=length(unique(aux$ecoregion_sim))*length(unique(aux$species)))
  EcoSppDOMParam_new <- rbind(aux, EcoSppDOMParam_new)
}
EcoSppDOMParam <- EcoSppDOMParam_new
  
EcoSppDOMParam <- dplyr::select(EcoSppDOMParam,ecoregion_sim,species, DOMpool, DecayRate, Amount, Q10)
colnames(EcoSppDOMParam) <- c("ecoregion","species", "DOMpool", "DecayRate", "AmountT0", "Q10")
EcoSppDOMParam <- EcoSppDOMParam[order( EcoSppDOMParam$ecoregion, EcoSppDOMParam$species, EcoSppDOMParam$DOMpool ),  ]

#Table EcoSppDOMParameter (values to be pasted in the ForC-succession input file)
write.table(EcoSppDOMParam, file="EcoSppDOMParam_MSCCentre.txt", sep = " ", quote=FALSE, row.names = FALSE, col.names = TRUE)
