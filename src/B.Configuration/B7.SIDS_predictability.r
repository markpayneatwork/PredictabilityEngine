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
library(sf)
load("objects/PredEng_config.RData")
source("src/B.Configuration/B0.Define_SST_data_srcs.r")

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
                       observations=SST_obs[[c("HadISST")]],
                       #CMIP5.models=CMIP5.mdls.l,    #Disable
                       NMME.models=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Drop NCEP forced model
pcfg@decadal.models <- hindcast_mdls[-which(names(hindcast_mdls)=="MPI-NCEP-forced")]

#If working locally, only keep the simplest two models
if(Sys.info()["nodename"]=="aqua-cb-mpay18") {
  pcfg@decadal.models <- pcfg@decadal.models[c(1,4)]
}

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@use.global.ROI <- FALSE
pcfg@global.res  <- 0.25

#Import EEZ's
load("resources/EEZs/EEZs_raw.RData")

#Restrict
eez.sel <- subset(eez.sf,Area_km2 >1e3)
EEZ.objs <- PredEng.list()
for(i in seq(nrow(eez.sel))) {
  this.sf <- eez.sel[i,]
  EEZ.objs[[i]] <- spatial.subdomain(name=as.character(this.sf$MRGID),boundary=as(this.sf$geometry,"Spatial"))
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
statsum.l <- PredEng.list()
#statsum.l[[1]] <- area.above.threshold(threshold=11)
statsum.l[[1]]  <- spatial.mean(data.type="means",use.anomalies=TRUE)
#statsum.l[[3]] <-isoline.lat(threshold=11)

#Merge it all in
names(statsum.l) <- sapply(statsum.l,slot,"name")
pcfg@summary.statistics <- statsum.l

#'========================================================================
# Output ####
#'========================================================================
#Write output files 
base.dir <- define_dir(pcfg@scratch.dir)
if(pcfg@use.global.ROI){
  log_msg("Writing Global Outputs...\n")
  #Write CDO grid descriptors
  this.ROI <- extend(pcfg@global.ROI,PE.cfg$ROI.extraction.buffer)
  analysis.grid.fname <- file.path(base.dir,PE.cfg$files$analysis.grid)
  griddes.txt <- griddes(this.ROI,res=pcfg@global.res)
  writeLines(griddes.txt,analysis.grid.fname)
  
  #Write regridded landmask
  regrid.landmask <- file.path(pcfg@scratch.dir,PE.cfg$files$regridded.landmask)
  exec(landmask.cmd <- cdo("-f nc",
                           csl("remapnn", pcfg@analysis.grid.fname),
                           pcfg@landmask,
                           regrid.landmask))
  
} else { #Loop over spatial subdomains
  for(sp in pcfg@spatial.subdomains){
    log_msg("Writing outputs Descriptor for %s...\n",sp@name)
    #Write CDO grid descriptors
    sp.dir <- define_dir(base.dir,sp@name)
    this.ROI <- extend(extent(sp),PE.cfg$ROI.extraction.buffer)
    griddes.txt <- griddes(this.ROI,res=pcfg@global.res) 
    analysis.grid.fname <- file.path(sp.dir,PE.cfg$files$analysis.grid)
    writeLines(griddes.txt,analysis.grid.fname)
    
    #Write regridded landmask
    regrid.landmask <- file.path(sp.dir,PE.cfg$files$regridded.landmask)
    exec(landmask.cmd <- cdo("-f nc",
                             csl("remapnn", analysis.grid.fname),
                             pcfg@landmask,
                             regrid.landmask))
  }  
}

#Output
save(pcfg,file="objects/configuration.RData")
save(pcfg,file=file.path(pcfg@scratch.dir,"configuration.RData"))

#'========================================================================
# Done
#'========================================================================
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
