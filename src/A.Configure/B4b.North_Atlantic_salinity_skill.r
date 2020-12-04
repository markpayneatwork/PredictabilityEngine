#'========================================================================
# B10a. North Atlantic Salinity skill
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Jul 26 14:50:42 2018
#
# Assesses the predictability of Salinity in the North Atlantic
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
cat(sprintf("\n%s\n","B4a. North Atlantic Salinity Skill"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
})
load(PE.cfg$path$datasrcs)

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "NA_Sal",
                       MOI=3,
                       average.months=FALSE,
                       clim.years=1981:2010,  
                       comp.years=1970:2015,
                       landmask="data_srcs/NMME/landmask.nc",
                       Observations=Sal.obs$EN4,
                       Decadal=Sal.Decadal,
                       calibrationMethods=c("anomaly","MeanVarAdj"))


#CMIP5 salinity
# pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="so")
# 

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-80,15,40,80)
pcfg@global.res  <- 1
pcfg@vert.range <- c(250,600)

#'========================================================================
# Extraction configuration ####
#'========================================================================


#'========================================================================
# Summary statistics ####
#'========================================================================
#Configure summary stats
statsum.l <- list()
statsum.l[[1]] <- pass.through(name="Anomaly",
                               desc="SST anomaly",
                               skill.metrics = "correlation",
                               calibration = c("anomaly","MeanVarAdj"),
                               realizations=1:4)

#'========================================================================
# Finish
#'========================================================================
set.configuration(pcfg)

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
