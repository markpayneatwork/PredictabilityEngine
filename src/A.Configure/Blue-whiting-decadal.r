#/*##########################################################################*/
#' Blue whiting Configuration
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Sun Sep  4 23:22:56 2016
#'
#' Configures a blue whiting  object
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
cat(sprintf("\n%s\n","Blue Whiting Decadal"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
})
these.srcs <- readRDS(PE.cfg$path$datasrcs)

# ========================================================================
# Generic Configuration
# ========================================================================
#Import configuration from B9
WGS2D.pcfg <- readRDS("scratch/Blue-whiting-WGS2D/configuration.rds")

#Global project configuration
pcfg <- PredEng.config(WGS2D.pcfg,
                       project.name= "Blue-whiting-decadal",
                       clim.years=1981:2010,  
                       comp.years=1980:2014,
                       calibrationMethods=c("MeanAdj"),
                       obs.only=FALSE)

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Setup persistence
pcfg@persistence.leads <- seq(pcfg@MOI-1,120,by=12)

#'========================================================================
# Data Sources ####
#'========================================================================
#Decadal salinity models
pcfg@Models <- 
  filter(these.srcs,
         group=="Sal.Decadal" | group=="CMIP6" & var=="so") %>%
  pull(sources) %>%
  PElst()

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Reduce size of global domain
pcfg@global.ROI <- extent(-25,0,50,65)
pcfg@global.res  <- 0.5

#'========================================================================
# Statistics ####
#'========================================================================
#Duplicate what's in B9
#but update to run across all data, not just the observations
for(i in seq(pcfg@statistics)) {
  this.stat <- pcfg@statistics[[i]] 
  this.stat@use.globalROI <- FALSE
  pcfg@statistics[[i]] <- this.stat
}

#Switch off many of the WGS2D features
pcfg@statistics[["SDM"]]@resources$WGS2D <- FALSE
pcfg@statistics[["SDM"]]@retain.field <- FALSE


#Reestablish grids for different resolutions
log10bath <- 
  raster("resources/BlueWhiting/ETOPO1_Bed_c_gmt4.grd") %>%
  crop(extent(pcfg@global.ROI)) %>%
  raster::aggregate(fact=pcfg@global.res/res(.),fun=mean)
log10bath <- log10(-log10bath)
if(!identical(res(log10bath)[1],pcfg@global.res)) stop("Mismatch in bathymetric resolution")
lat.rast <- log10bath    #Setup latitude raster
lat.rast[] <- yFromCell(lat.rast,1:ncell(log10bath))
pcfg@statistics[["SDM"]]@resources$pred.l <- 
       list(latitude=lat.rast,
            log10bath=log10bath)

#'========================================================================
# Output ####
#'========================================================================
pcfg <- set.configuration(pcfg)

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
