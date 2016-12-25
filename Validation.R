rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files

library(truncnorm)
library(ineq)
library(hydroGOF)

# setting environment -----------------------------------------------------

pr.dir <- "/Users/sergiomarconi/Documents/Classes/StatisticsForBiologicalSciences/3D-CMCC-Forest-Model"
#sites <- c("ITCastelporziano", "ITCollelongo", "ITRenon", "BEBrasschaat", "FIHyytiala")
sites <- c("FIHyytiala")

setwd(paste(pr.dir, "/src", sep=""))
curr.dir <- getwd()