#'========================================================================
# Calculate statistics
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed May 23 22:50:39 2018
#
# Calculates statistics across the entire range of data sources. Intended to
# work with statistics tools that generate both fields and singular values
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
# *  This version represents a significant departure from previous strategies.
#    The looping is now fully configurable - we are looping over "statistic atoms",
#    the basic indivisible unit of this analysis, consisting of the combination of
#    a datasrc, a stat and a spatial area. One atom gives one output file, but there
#    may be multiple atoms processed at a time. We can consider chunking in a future
#    implementaton if required.
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Calculate statistics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
library(tibble)
library(ncdf4)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.no <- 2
  debug.mode <- TRUE
  set.cdo.defaults("--silent --no_warnings")
  set.log_msg.silent()
} else {
  debug.mode <- FALSE
  #Taking inputs from the system environment
  cfg.no <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.no=="") stop("Cannot find LSB_JOBINDEX")
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}


#'========================================================================
# Setup ####
#'========================================================================
#Retrieve configurations
stats.cfg <- readRDS(file.path(PE.cfg$dirs$job.cfg,"Stats.rds"))
these.cfgs <- 
  filter(stats.cfg,cfg.id==cfg.no) %>%
  unnest(data)
stopifnot(nrow(these.cfgs)!=0)

#Directory setup
base.dir <-  pcfg@scratch.dir
obs.dir <- file.path(base.dir,pcfg@Observations@type,pcfg@Observations@name)
stat.dir <- define_dir(base.dir,PE.cfg$dirs$statistics)

#Setup observational climatology
clim.meta <- readRDS(file.path(obs.dir,PE.cfg$files$Obs.climatology.metadata))
obs.clim.l <- purrr::map(clim.meta$fname,brick)
names(obs.clim.l) <- sprintf("%02i",month(clim.meta$date))

#Setup landmask 
landmask <- raster(file.path(base.dir,PE.cfg$files$regridded.landmask))

#Check on configs
if(any(these.cfgs$src.type=="Persistence") & !pcfg@average.months & length(pcfg@MOI) >1 &
   any(!purrr::map_lgl(pcfg@statistics,slot,"use.full.field"))){
  stop("Don't know how to handle a persistence forecast for full
       field statistics in presence of multiple months")
}

#'========================================================================
# Calculation of statistics ####
#'========================================================================
# Loop over configurations  ------------------------------------------------------------
for(j in seq(nrow(these.cfgs))) {
  
  #Extract elements based on configuration
  this.cfg <- these.cfgs[j,]
  this.stat <- pcfg@statistics[[this.cfg$stat.name]]
  this.sp <- pcfg@spatial.domains[[this.cfg$sp.name]]
  this.src <- data.source(name=this.cfg$src.name,type=this.cfg$src.type) #Doesn't carry much useful info anyway

  #Load metadata
  #Remember that the metadata is preselected at the workload partitioning stage
  if(file.exists(this.cfg$metadat.path)) {
    metadat <-readRDS(this.cfg$metadat.path)
  } else {#Fail gracefully
    log_msg(sprintf("Error:Cannot find file %s.",this.cfg$metadat.path))
    stop()
  }

  log_msg("Processing '%s' statistic for '%s' subdomain, from '%s-%s' dataset...\n",
          this.stat@name,this.sp@name,this.src@type,this.src@name)
  
  
  #Configure the observation climatology
  if(pcfg@average.months | length(pcfg@MOI)==1) {
    metadat$which.clim <- 1  #Just use the value that is there
  } else {
    metadat$which.clim <- sprintf("%02i",month(metadat$date))
  }
  
  #Subset to make it run a bit quicker
  if(debug.mode) {
    metadat <- head(metadat,10)
  }
  
  #Setup the mask for the corresponding spatial boundary
  #based on the combination of the landmask and the spatial boundary mask
  combined.mask <- mask(landmask,this.sp@boundary,updatevalue=1)
  
  #Setup for looping
  file.res.l<- vector("list",nrow(metadat))
  pb <- progress_estimated(nrow(metadat))

  #Then loop over files
  for(i in seq(nrow(metadat))) {
    pb$tick()$print()
    m <- metadat[i,]
    f <- m$fname
    log_msg("Processing summary statistic %s, file %s...\n",
            this.stat@name,basename(f),silenceable = TRUE)    
    
    #Import model anom as a brick 
    #20190508 There was previously a problem working with a single layered brick in the raster
    #package. However, this seems to have been resolved now, so we can do everything as a simple
    #brick, which is the way God intended. 
    #20190811 Or maybe not... Maybe a solution is to convert it to a raster, if it only has one
    #layer
    mdl.anom <- brick(f)  
    if(nlayers(mdl.anom)==1) mdl.anom <- mdl.anom[[1]]
    
    #Choose whether we use full fields or anomalies
    if(this.stat@use.full.field) {      #Calculate the full field by adding in the appropriate climatology
      
      #Select the appropriate observation climatology
      obs.clim <- obs.clim.l[[m$which.clim]]
      
      #The resolutions of the observational climatology and the modelled anomaly match 
      #automatically, because an earlier step involves the interpolations of both the 
      #model output and observations onto the same analysis grid. This saves lots
      #of messing around with resolution adjustments etc
      
      #Build up the modelled value by combining the observational climatology
      #with the modelled anomaly.
      mdl.dat <- obs.clim + mdl.anom

    } else { #Use the anomaly
      mdl.dat <- mdl.anom
    }
    
    #Apply the masks to data
    masked.dat <- mask(mdl.dat,combined.mask,maskvalue=1)
    
    #And we're ready. Lets calculate some statistics
    res <- eval.stat(st=this.stat,dat=masked.dat) 
    
    #Need to add in realization labels
    #Realisations are only an issue when using them explicitly - otherwise we are
    #working from ensemble means, realisation means or observations => realization = NA
    #2019.11.01 Not sure we actually need the labels at all
    res$realization <- 1:nlayers(masked.dat)

    #Add in the metadata and store the results
    #Doing the bind diretly like this is ok when we are dealing with
    #rasterLayer fragments, but we will need to be caseful when dealing with 
    #bricks, for example
    file.res.l[[i]] <- 
      m %>% 
      mutate(res=list(res))
  }
  
  Sys.sleep(0.1)
  print(pb$stop())
  log_msg("\n")
  
  #Tidy up results a bit more
  res.out <- 
    bind_rows(file.res.l) %>% 
    as_tibble() %>%
    add_column(sp.subdomain=this.sp@name,
               stat.name=this.stat@name,
               .before=1) %>%
    dplyr::select(-which.clim) %>%
    unnest(res)

  #Store results
  saveRDS(res.out,file=file.path(stat.dir,this.cfg$res.fname))
} #/end loop over configs



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
