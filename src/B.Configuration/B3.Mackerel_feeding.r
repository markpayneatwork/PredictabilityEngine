#'========================================================================
# B9. Mackerel Summer Feeding
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Jul 26 14:50:42 2018
#
# Defines the configuration parameters for assessing the predictability of
# Mackerel summer feeding conditions and the possible predictability of the
# the Mackerel war

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
pcfg <- PredEng.config(project.name= "Mackerel_summer",
                       recalculate=TRUE,
                       MOI=8,
                       average.months=FALSE,
                       clim.years=1982:2005,  
                       comp.years=1970:2015,
                       landmask="data_srcs/NMME/landmask.nc",
                       Observations=SST_obs[[c("HadISST")]],#,
                       NMME=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Select decadal models
pcfg@Decadal <- SST.Decadal.production

#Select CMIP5 models
pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="tos")

#If working locally, only keep the simplest two models
if(Sys.info()["nodename"]=="aqua-cb-mpay18") {
  pcfg@Decadal <- pcfg@Decadal[c(1,4)]
}

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-70,0,50,80)
pcfg@global.res  <- 0.5
pcfg@retain.realizations <- TRUE

#Import EEZ's
load("resources/EEZs/EEZs.RData",verbose=TRUE)

#Split Greenlandic EEZ into an east and west EEZ, and carve off the top
eez.gland.full <- filter(eez.sf,str_detect(GeoName,c("Greenland")))
N.lim <- 70
greenland.EW.split <- -45
eez.gland.W <- st_crop(eez.gland.full,xmin=-180,xmax=greenland.EW.split,ymin=0,ymax=N.lim) %>%
                mutate(GeoName="West_Greenland")
eez.gland.E <- st_crop(eez.gland.full,xmin=greenland.EW.split,xmax=180,ymin=0,ymax=N.lim) %>%
                mutate(GeoName="East_Greenland")

#Extract Iceland
eez.iceland <- filter(eez.sf,str_detect(GeoName,c("Iceland")),Pol_type=="200NM") %>%
               mutate(GeoName="Iceland")


#Correct names and add to object
eez.sel <- rbind(eez.iceland, eez.gland.E,eez.gland.W) 
EEZ.objs <- list()
for(i in seq(nrow(eez.sel))) {
  this.sp <- eez.sel[i,]
  EEZ.objs[[i]] <- spatial.domain(name=as.character(this.sp$GeoName),
                                  desc=this.sp$GeoName,
                                  boundary=as(this.sp$geometry,"Spatial"))
}
names(EEZ.objs) <- eez.sel$GeoName
pcfg@spatial.domains <- EEZ.objs


#'========================================================================
# Extraction configuration ####
#'========================================================================


#'========================================================================
# Summary statistics ####
#'========================================================================
#Configure summary stats
statsum.l <- list()
statsum.l[[1]] <- threshold(name = "Area above 8.5 degrees",
                                 skill.metrics=c("cor","RMSE"),
                                 above=TRUE,
                                 use.full.field = TRUE,
                                 use.realmeans=TRUE,
                                 threshold=8.5)  #Based on Teunis' paper
statsum.l[[2]] <- pass.through(name="Temperature anomaly",
                               skill.metrics = "correlation",
                               use.globally=TRUE,
                               use.full.field = FALSE,
                               use.realmeans=TRUE)

#Setup habitat suitability functionality
habitat.mdl.dat <- readRDS("resources/Mackerel_summer_QR_values.rds")
resource.l <-  list(fun=approxfun(habitat.mdl.dat$temp,habitat.mdl.dat$value,rule=2))
habitat.fn <- function(dat,resources) {
  res <- dat
  res[] <- resources$fun(dat[])
  #Don't forget to un-log it, before using it!
  return(exp(res))
}

statsum.l[[3]] <-  habitat(name="Habitat",
                           fn=habitat.fn,
                           resources=resource.l,
                           use.realmeans=FALSE,
                           use.full.field = TRUE)

statsum.l[[4]] <-  new("habitat",
                       statsum.l[[3]],
                       name="Global Habitat",
                       use.globally=TRUE)

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
