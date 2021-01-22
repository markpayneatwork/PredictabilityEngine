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
cat(sprintf("\n%s\n","Blue Whiting NorCPM tests"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
})
load(PE.cfg$path$datasrcs)

# ========================================================================
# Generic Configuration
# ========================================================================
#Import configuration from B9
WGS2D.pcfg <- readRDS("scratch/Blue_whiting_WGS2D/configuration.rds")

#Global project configuration
pcfg <- PredEng.config(WGS2D.pcfg,
                       project.name= "Blue_whiting_NorCPM_tests",
                       clim.years=1981:2010,  
                       comp.years=1970:2015,
                       calibrationMethods=c("MeanAdj","MeanVarAdj"),
                       obs.only=FALSE)

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)
define_dir(pcfg@scratch.dir)

#'========================================================================
# Data Sources ####
#'========================================================================
#Decadal salinity models
pcfg@Decadal <- Sal.Decadal[grepl("^NorCPM",names(Sal.Decadal))]

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Reduce size of global domain
pcfg@global.ROI <- extent(-25,0,50,65)
pcfg@global.res  <- 1 #Reduce resolution to increase speed

#'========================================================================
# Statistics ####
#'========================================================================
#only run spatial averages
stat.l <- PElst()

#Average salinity
stat.l[["MeanSal"]]  <- spatial.mean(name="Mean-salinity",
                                     desc="Mean salinity",
                                     calibration=c("MeanAdj","MeanVarAdj"))

pcfg@statistics <- stat.l

#Drop point extractions
pcfg@pt.extraction <- tibble()

#'========================================================================
# Output ####
#'========================================================================
set.configuration(pcfg)

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
