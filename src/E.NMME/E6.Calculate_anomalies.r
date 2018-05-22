###########################################################################
# E6.Calculate_anomalies
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed May 16 09:37:48 2018
# 
# <Description>
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
cat(sprintf("\n%s\n","E6.Calculate_anomalies"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
load("objects/configuration.RData")
load("objects/setup.RData")

#==========================================================================
# Configure
#==========================================================================
base.dir <- file.path(pcfg@scratch.dir,"NMME")
lead.clim.dir <- define_dir(base.dir,"4.lead.clims")
anom.dir <- define_dir(base.dir,"A.anoms")

load(file.path(base.dir,"Anom_metadata.RData"))

set.debug.level(0) #Do all

#==========================================================================
# Calculate anomalies
#==========================================================================
#This is a pretty simple process - just delete everything from everything
log_msg("Calculating anomalies...\n")
pb <- progress_estimated(nrow(anom.meta))
for(i in seq(nrow(anom.meta))) {
  #Update progress bar
  pb$tick()$print()
  #Generate command
  anom.fname <- file.path(anom.dir,anom.meta$anom.fname[i])
  anom.cmd <- ncdiff("--netcdf4 --overwrite --history",
                     anom.meta$frag.fname[i],
                     file.path(lead.clim.dir,anom.meta$clim.fname[i]),
                     anom.fname)
  condexec(1,anom.cmd,silent=TRUE)
  
  # #Apply ncwa to remove degenerate dimensions
  # ncwa.cmd <- ncwa("--overwrite -a sst,S,L,M",
  #                  anom.fname,anom.fname)
  # condexec(1,ncwa.cmd)
  
}


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

