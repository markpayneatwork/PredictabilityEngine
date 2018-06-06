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
library(ncdf4)
load("objects/configuration.RData")

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  src.no <- 14
  set.debug.level(0)  #Non-zero lets us run with just a few points
  set.cdo.defaults("--silent --no_warnings")
  set.condexec.silent()
  set.log_msg.silent()
} else {
  #Taking inputs from the system environment
  src.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(src.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}

#Directory setup
base.dir <- pcfg@scratch.dir
obs.dir <- define_dir(file.path(base.dir,pcfg@observations@type,
                                pcfg@observations@name))
ind.dir <- define_dir(base.dir,"indicators")

#'========================================================================
# Setup ####
#'========================================================================
#Setup CMIP5 to spread across nodes
CMIP5.dirs <- dir(file.path(pcfg@scratch.dir,pcfg@CMIP5.models@source),
                 include.dirs = TRUE)
CMIP5.chunks <- unlist(rep(list(pcfg@CMIP5.models),length(CMIP5.dirs)))
for(i in seq(CMIP5.dirs)) {
  CMIP5.chunks[[i]]@name <- sprintf("%s-%s",CMIP5.chunks[[i]]@name,CMIP5.dirs[i])
  CMIP5.chunks[[i]]@source <- file.path(CMIP5.chunks[[i]]@source,CMIP5.dirs[i])
}

#Supported models
dat.srcs <- c(pcfg@decadal.hindcasts,pcfg@decadal.uninit,
              pcfg@NMME.models,
              pcfg@observations,pcfg@persistence,
              unlist(CMIP5.chunks))
src <- dat.srcs[[src.no]]
log_msg("Processing (%s) %s, number %i of %i available data sources\n\n",
        src@type,src@name,src.no,length(dat.srcs))

#Setup observation data
obs.clim.fname <- file.path(obs.dir,"obs_climatology.nc")
obs.clim.full <- raster(obs.clim.fname)
obs.clim <- crop(obs.clim.full,pcfg@ROI)  #Crop down to size

#Setup landmask by regridding
landmask <- raster(pcfg@landmask)

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
  log_msg("Processing '%s' indicator, number %i of %i...\n",
          ind@name,j,length(pcfg@indicators))
  
  #Load the appropriate metadata
  if(class(src)=="data.ensemble") { #Obviously only going to use ensmean data
    metadat.fname <- "Ensmean_metadata.RData"
  } else if(ind@data.type=="means") { #Use realmeans
    metadat.fname <- "Realmean_metadata.RData"
  } else if(ind@data.type=="realizations") { #Use individual realizations
    metadat.fname <- "Anom_metadata.RData"
  } else {
    stop("Unknown data type")
  }

  #Tweaks for NMME, CMIP
  if(src@type=="NMME") {
    #NMME  data is not separated by model name
    metadat.path <- file.path(base.dir,src@type,metadat.fname)
  } else if(src@type=="CMIP5") {
    #CMIP data are stored by chunks 
    metadat.path <- file.path(base.dir,src@source,metadat.fname)
  } else {
    metadat.path <- file.path(base.dir,src@type,src@name,metadat.fname)
  }
  
  metadat.varname <- load(metadat.path)
  metadat <- get(metadat.varname)
  
  #NMME is however processed individually, so we need to restrict the
  #metadata to the particular momdel
  if(src@type=="NMME" & class(src)=="data.source") {
     metadat <- subset(metadat,name==src@name)
  }
  
  #Subset the CMIP5 data
  if(src@type=="CMIP5") {
    metadat$CMIP5.chunk <- rep(sapply(CMIP5.chunks,slot,"name"),
                               length.out=nrow(metadat))
    metadat <- subset(metadat,CMIP5.chunk==src@name)
  }

  #Subset to make it run a bit quicker
  if(get.debug.level()!=0) {
    metadat <- metadat[1:10,]
  }
  
  #Setup for looping
  res.l <- list()
  pb <- progress_estimated(nrow(metadat))
  
  #Then loop over files
  for(i in seq(nrow(metadat))) {
    pb$tick()$print()
    m <- metadat[i,]
    f <- m$fname
    log_msg("Processing indicator %s, file %s...\n",
            ind@name,basename(f),silenceable = TRUE)    
    
    #Import model anom as a brick 
    #TODO
    #This is also a mess, due to the error found in raster. Currently hacking it
    if(ind@data.type=="realizations") {
      mdl.anom <- brick(f)  
      stop("working with realizations currently not supported")
      #The problem is essentially when we get to the storage of the results
      #but should be easy to solve
    } else  {
      mdl.anom <- raster(f) #Ideally this should be a brick, but that's not working for some reason 
    }
    
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
  #  masked <- setZ(masked,getZ(mdl.anom))
    
    # #Finally, drop layers that are completely blank
    # blank.layer <- cellStats(is.na(masked),sum)==ncell(masked)
    # no.blanks <- masked[[which(!blank.layer)]]
    
    #And we're ready. Lets calculate some indicators
    res <- eval.indicator(x=masked,m=ind) 
    
    #Add in the metadata and store the results
    #Doing the bind diretly like this is ok when we are dealing with
    #rasterLayer fragments, but we will need to be caseful when dealing with 
    #bricks, for example
    res.l[[i]] <- as.tibble(cbind(m,res))
  }
  
  Sys.sleep(0.1)
  print(pb$stop())
  log_msg("\n")
  
  #Tidy up results a bit more
  ind.res <- bind_rows(res.l) %>% 
              as.tibble() %>%
              add_column(indicator.name=ind@name,.before=1) %>%
              add_column(indicator.type=class(ind),.after=1) %>%
              add_column(indicator.data.type=ind@data.type,.after=2)
  #Store results
  save.fname <- gsub(" ","-",sprintf("%s_%s_%s.RData",src@type,src@name,ind@name))
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
