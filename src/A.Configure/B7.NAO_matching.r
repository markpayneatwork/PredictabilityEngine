#/*##########################################################################*/
#' NAO Matching
#' ==========================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' http://www.staff.dtu.dk/mpay
#'
#' 2020-12-29 15:34:13 CET
#'
#' Attempts to implement the NAO matching approach of Smith et al 2020 Nature
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
cat(sprintf("\n%s\n","NAO Matching"))
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
pcfg <- PredEng.config(project.name= "NAO_matching",
               MOI=c(12,1,2,3),  #December to March
               average.months=FALSE,
               clim.years=1960:2005,  
               comp.years=1960:2015,
               Observations=SLP.obs$HadSLP2,
               calibrationMethods="MeanAdj")

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Select decadal models
pcfg@Decadal <- SLP.Decadal

#Select CMIP5 models
#pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="tos")
pcfg@obs.only <- FALSE

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-30,-15,35,70)
pcfg@global.res  <- 1

#Polygons
sp.objs <- list()
sp.objs$"Azores" <- sfpolygon.from.extent(extent(-28,-20,36,40))
sp.objs$"Iceland" <- sfpolygon.from.extent(extent(-25,-16,63,70))

#Add to object
pcfg@spatial.polygons <- 
  sp.objs %>% 
  enframe(value="geometry") %>% 
  st_sf() 

#'========================================================================
# Statistics ####
#'========================================================================
#Configure stats
stat.l <- PElst()
stat.l$mean <-
  spatial.mean(name="MeanSLP",
               desc="Mean sea level pressure")

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
