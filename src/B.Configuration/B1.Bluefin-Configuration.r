#/*##########################################################################*/
#' Bluefin Predictability Common Elements
#' ==========================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' http://www.staff.dtu.dk/mpay
#'
#' Tue Jul 12 23:05:44 2016
#'
#' Defines a set of common-baseline elements for use across all pieces of code
#' in this codebase
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

#'========================================================================
# Initialise system
#'========================================================================
cat(sprintf("\n%s\n","Bluefin Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();

#Source the common elements
library(PredEng)
library(tibble)
load("objects/PredEng_config.RData")
source("src/B.Configuration/B0.Define_SST_data_srcs.r")

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "Bluefin",
               MOI=8,  #August
               average.months=FALSE,
               clim.years=1983:2005,  
               comp.years=1970:2012,
               landmask="data_srcs/NMME/landmask.nc",
               Observations=SST_obs[[c("HadISST")]],
               #CMIP5.models=CMIP5.mdls.l,    #Disable
               NMME=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Drop NCEP forced model
pcfg@Decadal <- hindcast_mdls[-which(names(hindcast_mdls)=="MPI-NCEP-forced")]

#If working locally, only keep the simplest two models
if(Sys.info()["nodename"]=="aqua-cb-mpay18") {
  pcfg@Decadal <- pcfg@Decadal[c(1,4)]
}

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@use.global.ROI <- FALSE
pcfg@global.ROI <- extent(-70,30,50,80)
pcfg@global.res  <- 0.5

#Polygons
sp.objs <- PredEng.list()
sp.objs$irminger.sea <- spatial.subdomain("Irminger.Sea",
                                     as.SpatialPolygons.matrix(rbind(c(-45,58),c(-45,66),
                                                   c(-20,66),c(-32,58))))
sp.objs$iceland.basin <- spatial.subdomain("Iceland.Basin",
                                      as.SpatialPolygons.matrix(rbind(c(-20,66),c(-32,58),
                                                    c(-15,58),c(-15,66))))
sp.objs$no.coast <- spatial.subdomain("Norwegian.Coast",
                                        as.SpatialPolygons.matrix(rbind(c(-5,62),c(10,62),
                                                                                c(20,70),c(20,73),
                                                                                c(12,73))))
sp.objs$s.iceland <- spatial.subdomain("South.of.Iceland",extent(-50,-10,55,70))

#Correct names and add to object
names(sp.objs) <- sapply(sp.objs,slot,"name")
pcfg@spatial.subdomains <- sp.objs


#'========================================================================
# Extraction configuration ####
#'========================================================================
extr <- list()
extr$spatial.forecasts <- as.Date("2018-08-15")

#A simple point-wise extraction point (corresponding to the point of capture)
pt <- data.frame(lat=65 +42/60, 
                 lon=-(30+50/60),
                 date=as.Date(c("2012-08-22","2014-08-15")))
pt$ID <- seq(nrow(pt))
coordinates(pt) <- ~ lon +lat
extr$spacetime.extraction <- pt

pcfg@extraction <- extr

#'========================================================================
# Summary statistics ####
#'========================================================================
#Configure summary stats
statsum.l <- PredEng.list()
statsum.l[[1]] <- area.above.threshold(threshold=11)
#statsum.l[[2]]  <- spatial.mean()
#statsum.l[[3]] <-isoline.lat(threshold=11)

#Set type of data to use for all
for(i in seq(statsum.l)){
  statsum.l[[i]]@data.type <- "means"
}

#Merge it all in
names(statsum.l) <- sapply(statsum.l,slot,"name")
pcfg@summary.statistics <- statsum.l

#'========================================================================
# Output ####
#'========================================================================
source("src/B.Configuration/B99.Configuration_wrapup.r")

#'========================================================================
# Done
#'========================================================================
#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nConfiguration complete.\n")

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
