#'========================================================================
# B10a. North Atlantic SST skill
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Jul 26 14:50:42 2018
#
# Assesses the predictability of SST in the North Atlantic
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","B9. Mackerel Summer Feeding"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Source the common elements
library(sf)
library(PredEng)
library(tibble)
library(raster)
library(tidyverse)
source("src/B.Configuration/B0.Define_common_data_srcs.r")

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "NA_SST_Predictability",
                       recalculate=FALSE,
                       MOI=8,
                       average.months=FALSE,
                       clim.years=1985:2004,  
                       comp.years=1985:2015,
                       landmask="data_srcs/NMME/landmask.nc",
                       Observations=SST_obs[[c("HadISST")]])#,
                       #CMIP5.models=CMIP5.mdls.l,    #Disable
                       #NMME=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Drop NCEP forced model
pcfg@Decadal <- SST.Decadal[-which(names(SST.Decadal)=="MPI-NCEP-forced")]

#If working locally, only keep the simplest two models
if(Sys.info()["nodename"]=="aqua-cb-mpay18") {
  pcfg@Decadal <- pcfg@Decadal[c(1,4)]
}

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@use.global.ROI <- TRUE
pcfg@global.ROI <- extent(-70,0,40,70)
pcfg@global.res  <- 0.5 #0.25
pcfg@retain.realizations <- TRUE

#'========================================================================
# Extraction configuration ####
#'========================================================================


#'========================================================================
# Summary statistics ####
#'========================================================================
#Configure summary stats
statsum.l <- list()
statsum.l[[1]] <- pass.through(name="Anomaly",
                               skill.metrics = "correlation",
                               use.globally=TRUE,
                               use.anomalies = TRUE,
                               use.realmeans=TRUE)

#Merge it all in
names(statsum.l) <- sapply(statsum.l,slot,"name")
pcfg@statistics <- statsum.l

#'========================================================================
# Done
#'========================================================================
#Output
source("src/B.Configuration/B99.Configuration_wrapup.r")

#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nConfiguration complete.\n")

# .............
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
# .............
