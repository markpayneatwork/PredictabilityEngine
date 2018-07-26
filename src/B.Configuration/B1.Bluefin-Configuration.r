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
load("objects/setup.RData")
source("src/B.Configuration/B0.Define_SST_data_srcs.r")

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- project.config(project.name= "Bluefin",
               MOI=8,  #August
               average.months=FALSE,
               clim.years=1983:2005,  
               comp.years=1970:2012,
               landmask="data_srcs/NMME/landmask.nc",
               observations=SST_obs[[c("HadISST")]],
               CMIP5.models=CMIP5.mdls,
#               decadal.uninit = uninit_mdls,
               NMME.models=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Drop NCEP forced model
pcfg@decadal.models <- hindcast_mdls[-which(names(hindcast_mdls)=="MPI-NCEP-forced")]

#If working locally, only keep the simplest two models
if(Sys.info()["nodename"]=="mpayne-Latitude-E7240") {
  pcfg@decadal.models <- pcfg@decadal.models[c(1,4)]
}

# #Add in the ensembles
# pcfg@decadal.models <- PredEng.list(c(pcfg@decadal.models,
#                                          data.ensemble(name="Decadal-ensmean",
#                                                        type="Decadal",
#                                                        members=pcfg@decadal.models)))
# pcfg@NMME.models <- PredEng.list(c(pcfg@NMME.models,
#                                    data.ensemble(name="NMME-ensmean",
#                                                  type="NMME",
#                                                  members=pcfg@NMME.models)))

#Add in a persistence forcast
# pcfg@persistence <- new("data.source",SST_obs[[c("HadISST")]],type="Persistence")

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-70,30,50,80)
pcfg@global.res  <- 0.5

#Polygons
sp.objs <- PredEng.list()
sp.objs$irminger.sea <- spatial.subdomain(name="Irminger Sea",
                                     polygon=as.SpatialPolygons.matrix(rbind(c(-45,58),c(-45,66),
                                                   c(-20,66),c(-32,58))))
sp.objs$iceland.basin <- spatial.subdomain(name="Icealand Basin",
                                      polygon=as.SpatialPolygons.matrix(rbind(c(-20,66),c(-32,58),
                                                    c(-15,58),c(-15,66))))
sp.objs$no.coast <- spatial.subdomain(name="Norwegian Coast",
                                        polygon=as.SpatialPolygons.matrix(rbind(c(-5,62),c(10,62),
                                                                                c(20,70),c(20,73),
                                                                                c(12,73))))
sp.objs$s.iceland <- spatial.subdomain(name="South of Iceland",
                                       ROI=extent(-50,-10,55,70))

#Add to object
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
pcfg@summary.statistics <- statsum.l

#'========================================================================
# Output ####
#'========================================================================
# # #Write CDO grid descriptor
# # writeLines(griddes(pcfg),pcfg@analysis.grid)
# 
# #Setup regridded landmask
# regrid.landmask <- file.path(pcfg@scratch.dir,"landmask_regridded.nc")
# exec(landmask.cmd <- cdo("-f nc", 
#                          csl("remapnn", pcfg@analysis.grid),
#                          pcfg@landmask, 
#                          regrid.landmask))
# pcfg@landmask <- regrid.landmask

#Output
save(pcfg,file="objects/configuration.RData")
save(pcfg,file=file.path(pcfg@scratch.dir,"configuration.RData"))

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
