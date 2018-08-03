#'##########################################################################
# E6.Calculate_anomalies
#'==========================================================================
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
#'##########################################################################

#'==========================================================================
# Initialise system ####
#'==========================================================================
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

#'==========================================================================
# Configure ####
#'==========================================================================
base.dir <- file.path(pcfg@scratch.dir,"NMME")
lead.clim.dir <- define_dir(base.dir,"4.lead.clims")
remap.dir <- define_dir(base.dir,"5.remap_wts")

load(file.path(base.dir,"Anom_metadata.RData"))
set.debug.level(Inf) #Do all
set.condexec.silent()
set.cdo.defaults("--silent --no_warnings -O")

#'==========================================================================
# Remapping weights ####
#'==========================================================================
#This is a pretty simple process - just subtract everything from everything
#But we also need to take care of the remapping as well, and this is a 
#good place to do it. Remapping weights are therefore setup first

log_msg("Calculating remapping weights...\n")

#Get list of files to act as representative sources
anom.meta$remapping.wts <- file.path(remap.dir,
                                     sprintf("%s.nc",anom.meta$name))
mdl.reps <- subset(anom.meta,year(start.date)==2011 ) %>%
            subset(!duplicated(name))

#Loop over models
for(m in seq(nrow(mdl.reps))) {
  condexec(1,wts.cmd <- cdo(csl("genbil",pcfg@analysis.grid),
                            mdl.reps$frag.fname[m],
                            mdl.reps$remapping.wts[m]))
}

#'==========================================================================
# Anomalies, remapping ####
#'==========================================================================
log_msg("Calculating anomalies...\n")
pb <- progress_estimated(nrow(anom.meta))
for(i in seq(nrow(anom.meta))) {
  #Update progress bar
  pb$tick()$print()
  am <- anom.meta[i,]
  
  #Calculate anomaly
  anom.temp <- tempfile(fileext = ".nc")
  # anom.cmd <- ncdiff("--netcdf4 --overwrite --history",
  #                    anom.meta$frag.fname[i],
  #                    file.path(lead.clim.dir,anom.meta$clim.fname[i]),
  #                    anom.temp)
  anom.cmd <- cdo("sub",
                     am$frag.fname,
                     file.path(lead.clim.dir,am$clim.fname),
                     anom.temp)
  
  condexec(1,anom.cmd)
  
  #Remap
  condexec(2,regrid.cmd <- cdo("-f nc",
                               csl("remap", pcfg@analysis.grid, am$remapping.wts),
                               anom.temp,am$fname))
  
}


#'==========================================================================
# Complete
#'==========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

#'-----------
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
#'-----------

