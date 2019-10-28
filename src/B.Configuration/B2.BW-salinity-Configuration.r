#/*##########################################################################*/
#' Blue whiting salinity Configuration
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' TSun Sep  4 23:22:56 2016
#'
#' Configures a blue whiting salinity object
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute" 
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
# - While this script contains reminants of RMarkdown, it is not in a state
#    where it can be compiled in a meaningful manner
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Blue whiting salinity Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();

#Source the common elements
library(PredEng)
library(tidyverse)
source("src/B.Configuration/B0.Define_common_data_srcs.r")

# ========================================================================
# Generic Configuration
# ========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "BW-Salinity",
                       MOI=3,  #March - ideally should be Feb-March-april average (??)
                       average.months=FALSE,
                       clim.years=1982:2005,  
                       comp.years=1982:2015,
                       landmask="data_srcs/NMME/landmask.nc")

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@use.global.ROI <- TRUE
pcfg@global.ROI <- extent(-25,0,40,65)
pcfg@global.res  <- 0.5
pcfg@vert.range <- c(250,600)

#Polygons
sp.objs <- list()
sp.objs$spawing.area <- spatial.domain("ROI",extent(-20,-5,50,60))

#Correct names and add to object
names(sp.objs) <- sapply(sp.objs,slot,"name")
pcfg@spatial.subdomains <- sp.objs

#'========================================================================
# Data Sources ####
#'========================================================================
#Define observational sources
pcfg@Observations <- Sal.obs$EN4

#Decadal salinity models
pcfg@Decadal <- Sal.Decadal

#CMIP5 salinity
pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="so")

#'========================================================================
# Statistics ####
#'========================================================================
#Configure stats
stat.l <- list()
stat.l[[1]]  <- spatial.mean(name="Mean Salinity",
                             use.full.field=TRUE)

#Setup Miesner & Payne habitat model
require(mgcv)
#Setup bathymetric grid 
#Note that we need to account for the extraction buffer here, to get a 
#perfect match with the rest of the data
log10bath <- raster("resources/BlueWhiting/ETOPO1_Bed_c_gmt4.grd") %>%
              crop(extend(pcfg@global.ROI,PE.cfg$misc$ROI.extraction.buffer)) %>%
              raster::aggregate(fact=pcfg@global.res/res(.),fun=mean)
log10bath <- log10(-log10bath)

#if(max(abs(extent(log10bath)[1:4]-pcfg@global.ROI[1:4]))>1e-9) stop("Mismatch in bathymetric dimensions")
if(!identical(res(log10bath)[1],pcfg@global.res)) stop("Mismatch in bathymetric resolution")

#Setup latitude raster
lat.rast <- log10bath
lat.rast[] <- yFromCell(lat.rast,1:ncell(log10bath))

#Setup prediction grid
pred.l <- list(latitude=lat.rast,
                       log10bath=log10bath)
pred.consts <- data.frame(doy=105,sol.el=0)

#Setup resource list
GAM.sdm.resources <- list(model=readRDS("resources/BlueWhiting/BW_GAM_SDM.rds"),
                          pred.l=pred.l,
                          pred.consts=pred.consts)

#Setup prediction function
GAM.sdm.fn <- function(dat,resources) {
  #Crop the prediction data down to the same scale as the values
  names(dat) <- "EN4.salinity"
  pred.dat <- brick(c(resources$pred.l,dat))
  p <- raster::predict(object=pred.dat,model=resources$model,fun=predict.gam,
                  const=resources$pred.consts, type="response")
  PA <- p > resources$model$threshold
  return(PA)
}
stat.l[[2]] <- habitat(name="SDM_realmean",
                       fn=GAM.sdm.fn,
                       resources=GAM.sdm.resources,
                       skill.metrics = "correlation",
                       use.full.field = TRUE,
                       use.realmeans=TRUE)

stat.l[[3]] <- habitat(name="SDM_realisations",
                       fn=GAM.sdm.fn,
                       resources=GAM.sdm.resources,
                       skill.metrics = "correlation",
                       use.full.field = TRUE,
                       use.realmeans = FALSE)


#Merge it all in
names(stat.l) <- sapply(stat.l,slot,"name")
pcfg@statistics <- stat.l


#'========================================================================
# Output ####
#'========================================================================
source("src/B.Configuration/B99.Configuration_wrapup.r")

# ========================================================================
# Done
# ========================================================================
#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete.\n")

#' -----------
#' <small>*This work by Mark R Payne is licensed under a  Creative Commons
#' Attribution-NonCommercial-ShareAlike 3.0 Unported License. 
#' For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
#' Basically, this means that you are free to "share" and "remix" for 
#' non-commerical purposes as you see fit, so long as you "attribute" me for my
#' contribution. Derivatives can be distributed under the same or 
#' similar license.*</small>
#'
#' <small>*This work comes with ABSOLUTELY NO WARRANTY or support.*</small>
#'
#' <small>*This work should also be considered as BEER-WARE. For details, see
#' http://en.wikipedia.org/wiki/Beerware*</small>
#' 
#' -----------
#
# Fin
