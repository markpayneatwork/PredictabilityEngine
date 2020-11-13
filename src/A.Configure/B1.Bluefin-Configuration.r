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

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
  library(tibble)
})
load(PE.cfg$path$datasrcs)

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "Bluefin",
               MOI=8,  #August
               average.months=FALSE,
               clim.years=1982:2005,  
               comp.years=1970:2015,
               landmask="data_srcs/NMME/landmask.nc",
               Observations=SST_obs[[c("HadISST")]],
               NMME=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Select decadal models
pcfg@Decadal <- SST.Decadal.production

#Select CMIP5 models
#pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="tos")

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-70,30,50,80)
pcfg@global.res  <- 0.5

#Polygons
sp.objs <- list()
sp.objs$"IrmingerSea" <-
  st_polygon(list(rbind(c(-45,58), c(-45,66), c(-20,66),
                        c(-32,58),c(-45,58))))

sp.objs$"IcelandBasin" <- 
  st_polygon(list(rbind(c(-20,66),c(-32,58),c(-15,58),
                        c(-15,66),c(-20,66))))

sp.objs$"NorwegianCoast" <-
    st_polygon(list(rbind(c(-5,62),c(10,62),c(20,70),
                          c(20,73),c(12,73),c(-5,62))))

sp.objs$"SouthOfIceland" <- sfpolygon.from.extent(extent(-50,-10,54,70))

#Add to object
pcfg@spatial.polygons <- 
  sp.objs %>% enframe(value="geometry") %>% st_sf()


#'========================================================================
# Extraction configuration ####
#'========================================================================
extr <- list()
extr$spatial.forecasts <- as.Date("2019-08-15")

#A simple point-wise extraction point (corresponding to the point of capture)
# pt <- data.frame(lat=65 +42/60, 
#                  lon=-(30+50/60),
#                  date=as.Date(c("2012-08-22","2014-08-15")))
# pt$ID <- seq(nrow(pt))
# coordinates(pt) <- ~ lon +lat
# extr$spacetime.extraction <- pt

#pcfg@extraction <- extr

#'========================================================================
# Statistics ####
#'========================================================================
#Configure stats
stat.l <- PElst()
stat.l[[1]] <- threshold(name="threshold",
                         desc="11 degree threshold",
                         threshold=11,
                         above=TRUE,
                         calibration = c("Mean adjusted"),
                         realizations=1:4)

#Merge it all in
pcfg@statistics <- stat.l

#'========================================================================
# Finish
#'========================================================================
set.configuration(pcfg)

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
