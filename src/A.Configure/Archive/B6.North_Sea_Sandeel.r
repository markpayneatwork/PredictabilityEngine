#'========================================================================
# B6.North Sea Sandeel
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Jul 26 14:50:42 2018
#
# Configures the parameters for looking at the predictability of North Sea
# Sandeel. Originally intended to look at temperatures in relation to
# Sandeel Phenology prediction, this could readily be extended to other
# aspects as well
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
cat(sprintf("\n%s\n","B6. North Sea Sandeel"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Source the common elements
library(PredEng)
library(tibble)
library(raster)
library(sf)
source("src/B.Configuration/B0.Define_common_data_srcs.r")

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "North_Sea_Sandeel",
                       MOI=3,
                       average.months=FALSE,
                       clim.years=1983:2010,  
                       comp.years=1970:2018,
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
pcfg@global.ROI <- extent(-4,11,51,60)
pcfg@global.res  <- 0.25
pcfg@retain.realizations <- TRUE

#Polygons
sp.objs <- list()
sp.objs$dogger <- spatial.domain("Dogger_Bank",
                                       as(extent(1,2.8,54.24,55.8),"SpatialPolygons"))
sp.objs$wadden <- spatial.domain("Wadden_Sea",
                                 as(extent(6.4,9.0,53.9,56.2),"SpatialPolygons"))

#Correct names and add to object
names(sp.objs) <- purrr::map_chr(sp.objs,slot,name="name")
pcfg@spatial.domains <- sp.objs

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

stat.l[[2]]  <- spatial.mean(name="Mean Anomaly",
                             use.full.field=FALSE)

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
