#/*##########################################################################*/
#' Calculate Indicators from the CMIP5 ensemble
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Tue Sep 13 21:50:06 2016
#'
#' Calculates the various indicators for the CMIP5 files
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
cat(sprintf("\n%s\n","Calculate CMIP5 indicators"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ClimateTools)
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
  # mdl.no <- 8
  set.run.level(0)  #0 complete fresh run
} else {
  #Taking inputs from the system environment
  # mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  # if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.run.level(0)  #0 complete fresh run
}

#Directory setup
base.dir <- file.path("processing",pcfg@name)
anom.dir <- file.path(base.dir,"CMIP5", "6.anom")
realmean.dir <- file.path(base.dir,"CMIP5","4.realmean")
out.dir <- define_dir(base.dir,"indicators")

# ========================================================================
# Setup 
# ========================================================================
#Setup observation data, if required
has.obs <- length(pcfg@observations)>0
if(has.obs) {
  obs.dir <- file.path(base.dir,pcfg@observations[[1]]@name)
  obs.clim.fname <- file.path(obs.dir,"obs_climatology.nc")
  obs.clim.full <- raster(obs.clim.fname)
  obs.clim <- crop(obs.clim.full,pcfg@ROI)  #Crop down to size
}

#Setup landmask by regridding
landmask.fname <- file.path(base.dir,"landmask_regridded.nc")
run_if(NA,landmask.cmd <- cdo("-f nc", 
                              csl("remapnn", pcfg@analysis.grid),
                              pcfg@landmask, landmask.fname))
landmask <- raster(landmask.fname)

#Get list of files to process - if there are observations
#defined, use the anomalies, otherwise need to adjust accordingly 
# and just use the modelled values (without any bias correction)
if(has.obs) { #use anomalies
  meta.df <- data.frame(fname=dir(anom.dir,pattern="*.nc",full.names = TRUE))
} else { #Use realisation means
  meta.df <- data.frame(fname=dir(realmean.dir,pattern="*.nc",full.names = TRUE))
}
meta.df$src <- gsub("^(.*?_.*?)_.*$","\\1",basename(meta.df$fname))

#Result storage
ind.l <- list()

#Setup a fake model object
mdl <-GCM(type="CMIP5",var=pcfg@CMIP5.var)

# ========================================================================
# Calculate indicators per file 
# ========================================================================
#Loop over anomaly files
for(i in seq(nrow(meta.df))) {
  log_msg("File %05i of %05i... ",i,nrow(meta.df))
  
  #Create a model object
  mdl@name <- meta.df$src[i]
  
  #Import model anom as a brick 
  f <- meta.df$fname[i]
  mdl.anom <- brick(f)
  
  #The resolutions of the observational climatology and the modelled anomaly match 
  #automatically, because an earlier step involves the interpolations of both the 
  #model output and observations onto the same analysis grid. This saves lots
  #of messing around with resolution adjustments etc
  
  #Note that differences in resolution can lead to differences in extent between
  #the products, even if they are extracted with the same basic ROI definition.
  #We therefore need to crop the data down to a common ROI before creating a
  #combined temperature object
  #mdl.temp <- obs.clim + crop(mdl.anom,pcfg@ROI)
  #Changes made where everything gets interpolated onto the same basic analysis
  #grid have largely rendered this step irrelevant
  
  #Build up the modelled temperature by combining the observational climatology
  #with the modelled anomaly - but only if we have observations in the first place
  if(has.obs) {
    mdl.temp <- obs.clim + mdl.anom
  } else {
    mdl.temp <- mdl.anom
  }
  
  #Apply the land mask 
  log_msg("Masking... ")
  masked <- mask(mdl.temp,landmask,maskvalue=1)
  
  #Set the dates of the raster to be processed to be the same as those in the
  #original source file - these processing steps don't always propigate them
  #correctly
  masked <- setZ(masked,mdl@date_fn(f))

  # #Finally, drop layers that are completely blank
  # blank.layer <- cellStats(is.na(masked),sum)==ncell(masked)
  # no.blanks <- masked[[which(!blank.layer)]]

  #And we're ready. Lets calculate some indicators
  log_msg("indicators... ")
  ind.l[[i]] <- lapply(pcfg@indicators,eval.indicator,x=masked)
  ind.l[[i]] <- do.call(rbind,ind.l[[i]])
  ind.l[[i]]$fname <- f
  
  log_msg("Done.\n")
}

# ========================================================================
# Aggregate and tidy-up 
# ========================================================================
#Aggregate indicator list
mdl.inds <- do.call(rbind,ind.l)

#Merge in metadata as required
mdl.inds$type <- "CMIP5"
mdl.inds$src <- gsub("^(.*?_.*?)_.*$","\\1",basename(mdl.inds$fname))
mdl.inds$forecast.init <- mdl.inds$date
mdl.inds$real <- NA
mdl.inds$fname <- NULL

#Save indicators
save(mdl.inds,file=file.path(out.dir,"CMIP5.RData"))

# ========================================================================
# Complete
# ========================================================================
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
