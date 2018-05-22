#/*##########################################################################*/
#' Calculate Metrics
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Wed May 25 08:27:23 2016
#'
#' Calculates the various metrics to be used in assessing the predictability of
#' Bluefin Tuna habitat
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
cat(sprintf("\n%s\n","Calculate Metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
load("objects/common_elements.RData")

library(raster)
library(stringr)
library(reshape2)

# ========================================================================
# Configuration
# ========================================================================
anom.dir <- "data/NMME/6.anom/"
load("objects/metric_functions.RData")

# ========================================================================
# Loop over anomaly files
# ========================================================================
#Get list of anomaly files
anom.fnames <- dir(anom.dir,pattern="anom.nc",full.names = TRUE)
f.df <- data.frame(fname=anom.fnames,
                   prefix=str_match(basename(anom.fnames),"^.*?_(.*?)_anom.nc$")[,2])

#Misc setup
obs.clim <- raster(obs.clim.fname)
met.l <- list()

#Setup landmask
landmask <- raster("data/NMME/landmask.nc")
res.landmask <- res(landmask)
res.obs <- res(obs.clim)
if(!identical(res.landmask,res.obs)) {
  disagg.fact <- res.landmask/res.obs
  landmask <- disaggregate(landmask,disagg.fact,method="")
} 
landmask.local <- crop(landmask,union.ROI)

#Loop over anomaly files
for(i in seq(nrow(f.df))) {
  log_msg("%s... ",f.df$prefix[i])
  
  #Import NMME anom brick 
  NMME.anom.raw<- brick(f.df$fname[i])
  
  #The resolutions of the climatology and the anomaly don't match, so we're
  #going to have make sure that they do. We do this by interpolating to the
  #observation grid, which is typically finer
  log_msg("Disaggregating... ")
  res.NMME <- res(NMME.anom.raw)
  res.obs <- res(obs.clim)
  disagg.fact <- res.NMME/res.obs
  NMME.anom <- disaggregate(NMME.anom.raw,disagg.fact,method="bilinear")

  #Note that differences in resolution can lead to differences in extent between
  #the products, even if they are extracted with the same basic ROI definition.
  #We therefore need to crop the data down to a common ROI before creating a
  #combined temperature object
  NMME.temp <- crop(obs.clim,union.ROI) + crop(NMME.anom,union.ROI)
  
  #Apply the NMME land mask - again this requires some resolution adjustment
  #and cropping
  log_msg("Masking... ")
  masked <- mask(NMME.temp,landmask.local,maskvalue=1)
  masked <- setZ(masked,getZ(NMME.anom.raw))
  
  # #Finally, drop layers that are completely blank
  # blank.layer <- cellStats(is.na(masked),sum)==ncell(masked)
  # no.blanks <- masked[[which(!blank.layer)]]

  #And we're ready. Lets calculate some metrics
  log_msg("Metrics... ")
  met.l$area[[i]] <- area.above.thresh(masked,temp.thresh,area.ROI)
  met.l$area[[i]]$src <- f.df$prefix[i] 
  
  met.l$mean[[i]] <- mean.temp(masked,ts.ROI)
  met.l$mean[[i]]$src <- f.df$prefix[i] 

  log_msg("Done.\n")
}

#Aggregate metrics
NMME.mets <- lapply(met.l,function(l) do.call(rbind,l))
NMME.mets <- do.call(rbind,NMME.mets)

#Touch up dates 
NMME.mets$forecast.init <- NMME.mets$z
NMME.mets$forecast.date <- ceiling.month(NMME.mets$z,MoI) + days(15)
NMME.mets$z <- NULL

#Calculate an ensemble mean

# ========================================================================
# Complete
# ========================================================================
#Save metrics
save(NMME.mets,file="objects/metrics_NMME.RData")

#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

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
