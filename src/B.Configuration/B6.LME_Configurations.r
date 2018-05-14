#/*##########################################################################*/
#' LME Configurations
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jan 12 20:43:36 2017
#'
#' Configures the predictability engine for performing a systematic processing
#' of the LME definitions
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
cat(sprintf("\n%s\n","LME Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();

#Source the common elements
library(PredEng)
library(ClimateTools)
source("src/B.Configuration/B0.Define_SST_data_srcs.r")
library(maptools)   #For importing shapefiles

# ========================================================================
# Generic Configuration
# ========================================================================
log_msg("Preparing generic configuration...\n")
#Global project configuration
pcfg <- config(name= "LME",
               res=0.25,
               MOI=1:12)

#Exclude the models from Daniela to start with
pcfg@hindcast.models <- hindcast_mdls[c("MPI-MR","IPSL","GFDL")]

#Indicator is to calculate the time series 
pcfg@indicators <- list(new("spatial.mean",name="spatial_mean"))

# ========================================================================
# Write one configuration per LME
# ========================================================================
#First import the LME polygons
LME.polys <- readShapePoly("data_srcs/LME_definitions/LME66.shp")

#Then loop over the various LMEs
for(i in seq(LME.polys)) {
  #Make storage space
  LME.name <- gsub(" ","_",LME.polys[i,]$LME_NAME)
  pcfg@name <- sprintf("%02i_%s",i,LME.name)
  LME.dir <- define_dir(file.path("processing","LME",pcfg@name))
  log_msg("Preparing configuration of LME %s...\n",pcfg@name)
    
  #Define paths
  pcfg@analysis.grid <- file.path(LME.dir,"analysis.grid")
  
  #Apply the polygon in the appropriate places
  LME.poly <- as(LME.polys[i,],"SpatialPolygons")
  pcfg@indicators[[1]]@poly.ROI <- LME.poly

  #Update everything
  pcfg@ROI <- extend(merge(pcfg),2)
  pcfg <- update(pcfg)

  #Write CDO grid descriptor
  writeLines(griddes(pcfg),pcfg@analysis.grid)
  
  #Output configuration
  save.image(file=file.path(LME.dir,"configuration.RData"))
  
  #Save the first LME in the standard location so that it works ok
  #with the other scripts (although there is no guarantee for that, as
  #many of the variables have not been defined properly)
  if(i==1) {
    save.image(file="objects/configuration.RData")
  }
}

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
