#'========================================================================
# Pointwise Extraction
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed May 23 22:50:39 2018
#
# Extracts values at specified space-time locations from the database
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Pointwise extraction"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
library(tibble)
library(lubridate)
library(ncdf4)
load("objects/configuration.RData")

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  src.no <- 4
  set.debug.level(0)  #Non-zero lets us run with just a few points
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
res.dir <- define_dir(base.dir,"pointwise")

#'========================================================================
# Setup ####
#'========================================================================
#Supported models
dat.srcs <- c(pcfg@decadal.hindcasts,pcfg@decadal.uninit,
              pcfg@NMME.models,pcfg@observations)
src <- dat.srcs[[src.no]]
log_msg("Processing (%s) %s, number %i of %i available data sources\n\n",
        src@type,src@name,src.no,length(dat.srcs))

#Setup observation data
obs.clim.fname <- file.path(obs.dir,"obs_climatology.nc")
obs.clim.full <- raster(obs.clim.fname)
obs.clim <- crop(obs.clim.full,pcfg@ROI)  #Crop down to size

#Load the appropriate metadata
#At the moment we don't support anomalies, although it is obviously
#fairly easy to setup
if(class(src)=="data.ensemble") { #Obviously only going to use ensmean data
  metadat.fname <- "Ensmean_metadata.RData"
} else  { #Use realmeans
  metadat.fname <- "Realmean_metadata.RData"
}
metadat.varname <- load(file.path(base.dir,src@type,src@name,metadat.fname))
metadat.all <- get(metadat.varname)

#Now we need to filter out the long list of all possible files to only
#look at the ones of interest. We assert a precision here corresponding
#to within a month ie if the forecast and the extraction point are in the 
#same month, it's a match. This take care of this by rounding all dates to 
#the floor
metadat.all$date.floor <- floor_date(metadat.all$date,"month")
extract.spdf <- pcfg@spacetime.extraction
extract.spdf$date.floor=floor_date(extract.spdf$date,"month")
extract.spdf$point.index <- seq(nrow(extract.spdf))
metadat <- subset(metadat.all,date.floor %in% extract.spdf$date.floor)

#Filter to make debugging easier
if(get.debug.level()!=0) {
  metadat <- metadat[1:10,]
}

#'========================================================================
# Extract data ####
#'========================================================================
#Setup for looping
res.l <- list()
pb <- progress_estimated(nrow(metadat))

#Then loop over files
for(i in seq(nrow(metadat))) {
  pb$tick()$print()
  m <- metadat[i,]
  f <- m$fname
  log_msg("Processing file %i of %i, %s...\n",
          i,nrow(metadat),basename(f),silenceable = TRUE)    
  
  #Import model anom as a brick 
  #TODO
  mdl.anom <- raster(f) #Ideally this could be a brick, but that's not working for some reason 
  
  #The resolutions of the observational climatology and the modelled anomaly match 
  #automatically, because an earlier step involves the interpolations of both the 
  #model output and observations onto the same analysis grid. This saves lots
  #of messing around with resolution adjustments etc
  
  #Build up the modelled value by combining the observational climatology
  #with the modelled anomaly.
  mdl.val <- obs.clim + mdl.anom
  
  #Then we need to make sure that we only extract the data from the month for which 
  #we have created the raster object
  matching.spdf <- subset(extract.spdf,date.floor==m$date.floor)

  #Now do the extractions
  res <- raster::extract(mdl.val,matching.spdf,sp=TRUE,
                         buffer=100*10^3,fun=mean,na.rm=TRUE) #Average over x km radius
  
  #Add in the metadata and store the results
  #Doing the bind diretly like this is ok when we are dealing with
  #rasterLayer fragments, but we will need to be caseful when dealing with 
  #bricks, for example
  res.l[[i]] <- cbind(m,tibble(point.index=res$point.index,value=res$layer))
}

Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

#Tidy up results a bit more
matchup.res <- bind_rows(res.l) %>% 
  as.tibble() 

#Store results
save.fname <- gsub(" ","-",sprintf("%s_matchups.RData",src@name))
save(matchup.res,file=file.path(res.dir,save.fname))

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
