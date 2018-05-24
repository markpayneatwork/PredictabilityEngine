#'========================================================================
# Calculate indicators
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed May 23 22:50:39 2018
#
# Calculates indicators across the entire range of data sources
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
# *  We choose to parallelise over datasources, rather than over indicators,
#    which would be an alternative structure. The logic behind this is that
#    not all data sets are available on one machine at the same time, due to
#    storage and practical limitations - however, we can always apply the
#    indicators (I hope). Thus, it makes more sense to parallelise over datasources
#    and then loop over indicators.
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Calculate_indicators"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
library(tibble)
load("objects/configuration.RData")

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  src.no <- 12
  set.debug.level(1)  #Non-zero lets us run with just a few points
} else {
  #Taking inputs from the system environment
  src.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(src.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}

#Directory setup
base.dir <- pcfg@scratch.dir
obs.dir <- file.path(base.dir,pcfg@observations[[1]]@name)
ind.dir <- define_dir(base.dir,"indicators")

#'========================================================================
# Setup ####
#'========================================================================
#Supported models
dat.srcs <- c(pcfg@DCPP.hindcasts,pcfg@DCPP.uninit,pcfg@NMME.models,pcfg@CMIP5.models)
src <- dat.srcs[[src.no]]
log_msg("Processing (%s) %s, number %i of %i available data sources\n\n",
        src@type,src@name,src.no,length(dat.srcs))

#Setup observation data
obs.clim.fname <- file.path(obs.dir,"obs_climatology.nc")
obs.clim.full <- raster(obs.clim.fname)
obs.clim <- crop(obs.clim.full,pcfg@ROI)  #Crop down to size

#Setup landmask by regridding
landmask.fname <- file.path(base.dir,"landmask_regridded.nc")
exec(landmask.cmd <- cdo("-f nc", 
                         csl("remapnn", pcfg@analysis.grid),
                         pcfg@landmask, landmask.fname))
landmask <- raster(landmask.fname)

#Result storage
ind.l <- list()

#'========================================================================
# Calculate indicators ####
#'========================================================================
#Outer loop is over the indicators. This is probably not the most effective
#strategy, as it involves some duplication around the calculation of the
#input fields. However, this should be relatively minor and is more than 
#compensated for by the fact that it is much simpler programmatically.
for(j in seq(pcfg@indicators)) {
  ind <- pcfg@indicators[[j]]
  log_msg("Processing %s indicator, number %i of %i...\n",ind@name,j,length(pcfg@indicators))
  
  #Load the appropriate metadata
  if(src@type=="ensmean") {
    metadat.fname <- file.path(src@source,"Ensmean_metadata.RData")
  } else if(ind@data.type=="means") {
    metadat.fname <- file.path(src@source,"Realmean_metadata.RData")
  } else if(ind@data.type=="realizations") {
    metadat.fname <- file.path(src@source,"Anom_metadata.RData")
  } else {
    stop("Unknown data type")
  }
  metadat.varname <- load(file.path(base.dir,metadat.fname))
  metadat <- get(metadat.varname)
  
  if(get.debug.level()!=0) {
    metadat <- metadat[1:10,]
  }
  
  #Setup for looping
  res.l <- list()
  pb <- progress_estimated(nrow(metadat))
  
  #Then loop over  files
  for(i in seq(nrow(metadat))) {
    pb$tick()$print()
    
    #Import model anom as a brick 
    m <- metadat[i,]
    f <- m$fname
    mdl.anom <- raster(f)  #Ideally this should be a brick, but that's not working for some reason
    
    #The resolutions of the observational climatology and the modelled anomaly match 
    #automatically, because an earlier step involves the interpolations of both the 
    #model output and observations onto the same analysis grid. This saves lots
    #of messing around with resolution adjustments etc
    
    #Build up the modelled value by combining the observational climatology
    #with the modelled anomaly.
    mdl.val <- obs.clim + mdl.anom
    
    #Apply the land mask 
    masked <- mask(mdl.val,landmask,maskvalue=1)
    
    #Set the dates of the raster to be processed to be the same as those in the
    #original source file - these processing steps don't always propigate them
    #correctly
    masked <- setZ(masked,src@date_fn(f))
    
    # #Finally, drop layers that are completely blank
    # blank.layer <- cellStats(is.na(masked),sum)==ncell(masked)
    # no.blanks <- masked[[which(!blank.layer)]]
    
    #And we're ready. Lets calculate some indicators
    res <- eval.indicator(x=masked,m=ind)  %>%
            add_column(indicator=ind@name,.before=1) 
    
    #Add in the metadata and store the results
    #Doing the bind diretly like this is ok when we are dealing with
    #rasterLayer fragments, but we will need to be caseful when dealing with 
    #bricks, for example
    res.l[[i]] <- bind_cols(m,res)  
  }
  
  Sys.sleep(0.1)
  print(pb$stop())
  log_msg("\n")
  
  #Store results
  ind.res <- bind_rows(res.l) %>% as.tibble()
  save.fname <- gsub(" ","-",sprintf("%s_%s.RData",src@name,ind@name))
  save(ind.res,file=file.path(ind.dir,save.fname))
}


#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# .............
# This work by Mark R Payne is licensed under a  Creative Commons
# Attribution-NonCommercial-ShareAlike 3.0 Unported License.
# For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
# Basically, this means that you are free to "share" and "remix" for
# non-commerical purposes as you see fit, so long as you "attribute" me for my
# contribution. Derivatives can be distributed under the same or
# similar license.
#
# This work comes with ABSOLUTELY NO WARRANTY or support.
#
# This work should also be considered as BEER-WARE. For details, see
# http://en.wikipedia.org/wiki/Beerware
# .............
