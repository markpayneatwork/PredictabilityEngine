###########################################################################
# Visualise_model_shock
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed May 16 10:40:36 2018
# 
# Plots the time series of climatolgies and therefore the model shock process
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute" 
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
###########################################################################

#==========================================================================
# Initialise system
#==========================================================================
cat(sprintf("\n%s\n","Visualise_model_shock"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
library(raster)
library(ggplot2)
library(reshape2)
load("objects/configuration.RData")
load("objects/setup.RData")

#==========================================================================
# Configure
#==========================================================================
base.dir <- file.path(pcfg@scratch.dir,"NMME")
lead.clim.dir <- define_dir(base.dir,"4.lead.clims")

load(file.path(base.dir,"Anom_metadata.RData"))

set.debug.level(0) #Do all

#==========================================================================
# Setup
#==========================================================================
#Get list of all climatologies
clim.data <- unique(anom.meta[,c("model","lead","clim.fname")])

#Make some storage space
clim.data$mean.temp <- 0.0

#==========================================================================
# Extract data
#==========================================================================
log_msg("Loading climatology data...\n")
pb <- progress_estimated(nrow(clim.data))
for(i in seq(nrow(clim.data))) {
  #Update progress bar
  pb$tick()$print()
  #Setup raster
  clim.r <- raster(file.path(lead.clim.dir,clim.data$clim.fname[i]))
  clim.data$mean.temp[i] <- cellStats(clim.r,"mean",na.rm=TRUE)

}

#==========================================================================
# Visualise
#==========================================================================
dat.mat <- acast(clim.data,model~lead,value.var = "mean.temp")
anom.mat <- sweep(dat.mat,1,dat.mat[,1],"-")
plt.dat <- melt(anom.mat)
g <- ggplot(plt.dat,aes(x=Var2,y=value,colour=Var1))+
      geom_line()+geom_point()+
      labs(x="Lead time (months)",colour="Model",y="Drift relative to lead = 0.5 months (deg C)")+
  theme_bw()
print(g)

#==========================================================================
# Complete
#==========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# -----------
# This work by Mark R Payne is licensed under a  Creative Commons
# Attribution-NonCommercial-ShareAlike 3.0 Unported License. 
# For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
# Basically, this means that you are free to "share" and "remix" for 
# non-commerical purposes as you see fit, so long as you "attribute" me for my
# contribution. Derivatives can be distributed under the same or 
# similar license.
#
# This work comes with ABSOLUTELY NO WARRANTY or support.
#
# This work should also be considered as BEER-WARE. For details, see
# http://en.wikipedia.org/wiki/Beerware
# -----------

