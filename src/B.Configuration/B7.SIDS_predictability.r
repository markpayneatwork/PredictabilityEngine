#'========================================================================
# B7.SIDS_predictability
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Jul 26 14:50:42 2018
#
# Defines the configuration parameters for SIDS predictability study
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
cat(sprintf("\n%s\n","B7.SIDS_predictability"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Source the common elements
library(PredEng)
library(tibble)
library(raster)
source("src/B.Configuration/B0.Define_common_data_srcs.r")

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "SIDS_Predictability",
                       MOI=1:12,
                       average.months=FALSE,
                       clim.years=1983:2010,  
                       comp.years=1970:2012,
                       landmask="data_srcs/NMME/landmask.nc",
                       Observations=SST_obs[[c("HadISST")]],
                       #CMIP5.models=CMIP5.mdls.l,    #Disable
                       NMME=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Select decadal models
pcfg@Decadal <- SST.Decadal.production

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@use.global.ROI <- TRUE
pcfg@global.ROI <- extent(-180,180,-90,90)
pcfg@global.res  <- 1 #0.25
pcfg@retain.realizations <- FALSE

#Import EEZ's
load("resources/EEZs/EEZs.RData")

#Modifications using sf
# eez.sel <- subset(eez.sf,Area_km2 >1e3)
# EEZ.objs <- PredEng.list()
# for(i in seq(nrow(eez.sel))) {
#   this.sf <- eez.sel[i,]
#   EEZ.objs[[i]] <- spatial.subdomain(name=as.character(this.sf$MRGID),boundary=as(this.sf$geometry,"Spatial"))
# }

#Exclude small EEZs
eez.sel <- subset(eez.sp,Area_km2 >1e3)
EEZ.objs <- list()
for(i in seq(nrow(eez.sel))) {
  this.sp <- eez.sel[i,]
  EEZ.objs[[i]] <- spatial.domain(name=as.character(this.sp$MRGID),
                                     desc=this.sp$GeoName,
                                     boundary=as(this.sp,"SpatialPolygons"))
}


#Correct names and add to object
names(EEZ.objs) <- eez.sel$MRGID
pcfg@spatial.subdomains <- EEZ.objs

#'========================================================================
# Extraction configuration ####
#'========================================================================


#'========================================================================
# Summary statistics ####
#'========================================================================
#Configure summary stats
stat.l <- list()
stat.l[[1]]  <- spatial.mean(name="Mean Temperature",
                             use.full.field=TRUE)

stat.l[[2]] <- pass.through(name="Temperature anomaly",
                            skill.metrics = "correlation",
                            use.globally=TRUE,
                            use.full.field = FALSE,
                            use.realmeans=TRUE)

#Merge it all in
names(stat.l) <- sapply(stat.l,slot,"name")
pcfg@statistics <- stat.l


#'========================================================================
# Done
#'========================================================================
#Output
source("src/B.Configuration/B99.Configuration_wrapup.r")

#As this project is so big, we also need to partition the collation
#process as well
#cfgs <- partition.workload(pcfg,NA,space.partition = TRUE)


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