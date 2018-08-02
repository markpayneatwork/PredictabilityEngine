#'========================================================================
# Calculate summary statistics
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed May 23 22:50:39 2018
#
# Calculates summary statistics across the entire range of data sources
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
# *  We choose to parallelise over datasources, rather than over summary statistics,
#    which would be an alternative structure. The logic behind this is that
#    not all data sets are available on one machine at the same time, due to
#    storage and practical limitations - however, we can always apply the
#    summary statistics (I hope). Thus, it makes more sense to parallelise over datasources
#    and then loop over summary statistics.
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Calculate_summary statistics"))
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
load("objects/PredEng_config.RData")

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  src.no <- 17  #1,2,10,15,17
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

#'========================================================================
# Divide work ####
#'========================================================================
#Supported data sources
dat.srcs.l <- c(pcfg@decadal.models,
              pcfg@NMME.models,
              pcfg@observations,
              pcfg@CMIP5.models)
dat.srcs <- tibble(src.type=sapply(dat.srcs.l,slot,"type"),
                       src.name=sapply(dat.srcs.l,slot,"name"))
dat.srcs <- rbind(dat.srcs,
                  tibble(src.type=c("Decadal","NMME"),src.name=PE.cfg$files$ensmean.name),
                  tibble(src.type="Persistence",src.name=pcfg@observations@name))
dat.srcs$src.id <- seq(nrow(dat.srcs))

#Supported spatial subdomains
if(pcfg@use.global.ROI) {
  sp.subdomains <- ""
} else {
  sp.subdomains <- names(pcfg@spatial.subdomains)
}

#Do the expansion
work.cfg <- expand.grid(src.id=dat.srcs$src.id,
                        sp=sp.subdomains) %>%
            left_join(dat.srcs,by="src.id") %>%
            as.tibble()
this.cfg <- work.cfg[src.no,]
this.sp <- pcfg@spatial.subdomains[[this.cfg$sp]]

log_msg("Processing (%s) %s for %s subdomain, number %i of %i configurations.\n\n",
        this.cfg$src.type,this.cfg$src.name,this.cfg$sp,src.no,nrow(work.cfg))

if(this.cfg$src.type=="Persistence" & !pcfg@average.months & length(pcfg@MOI) >1 &
   any(!sapply(pcfg@summary.statistics,slot,"use.anomalies"))){
  stop("Don't know how to handle a persistence forecast for full
       field summary statistics in presence of multiple months")
}

#'========================================================================
# Setup ####
#'========================================================================
#Directory setup
base.dir <- file.path(pcfg@scratch.dir,this.sp@name)
obs.dir <- file.path(base.dir,pcfg@observations@type,pcfg@observations@name)
sumstat.dir <- define_dir(base.dir,"Summary.statistics")

#Setup observational climatology
load(file.path(obs.dir,PE.cfg$files$Obs.climatology.metadata))
obs.clim.l <- lapply(clim.meta$fname,raster)
names(obs.clim.l) <- sprintf("%02i",month(clim.meta$date))

#Setup landmask by regridding
#landmask <- raster(pcfg@landmask)

#Result storage
sum.stats.l <- list()

#'========================================================================
# Calculate summary statistics ####
#'========================================================================
#Outer loop is over the summary statistics This is probably not the most effective
#strategy, as it involves some duplication around the calculation of the
#input fields. However, the summary statistics, in principle, determine the type
#of data that should be used as an input (i.e. realmeans, realizations etc), so 
#it makes most sense to it this way around.

for(j in seq(pcfg@summary.statistics)) {
  sumstat <- pcfg@summary.statistics[[j]]
  log_msg("Processing '%s' summary statistic, number %i of %i...\n",
          sumstat@name,j,length(pcfg@summary.statistics))
  
  #Load the appropriate metadata
  if(this.cfg$src.name==PE.cfg$files$ensmean.name) { #Obviously only going to use ensmean data
    metadat.fname <- PE.cfg$files$realmean.meta
  } else if(sumstat@data.type=="means") { #Use realmeans
    metadat.fname <- PE.cfg$files$realmean.meta
  } else if(sumstat@data.type=="realizations") { #Use individual realizations
    metadat.fname <- PE.cfg$files$anom.meta
  } else {
    stop("Unknown data type")
  }

  #Load Metadata
  metadat.path <- file.path(base.dir,this.cfg$src.type,this.cfg$src.name,metadat.fname)
  metadat.varname <- load(metadat.path)
  metadat <- get(metadat.varname)
  
  #Configure the observation climatology
  if(pcfg@average.months | length(pcfg@MOI)==1) {
    metadat$which.clim <- 1  #Just use the value that is there
  } else {
    metadat$which.clim <- sprintf("%02i",month(metadat$date))
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
    log_msg("Processing summary statistic %s, file %s...\n",
            sumstat@name,basename(f),silenceable = TRUE)    
    
    #Import model anom as a brick 
    #TODO
    #This is also a mess, due to the error found in raster. Currently hacking it
    if(sumstat@data.type=="realizations") {
      mdl.anom <- brick(f)  
      stop("working with realizations currently not supported")
      #The problem is essentially when we get to the storage of the results
      #but should be easy to solve
    } else  {
      mdl.anom <- raster(f) #Ideally this should be a brick, but that's not working for some reason 
    }
    
    #Choose whether we use full fields or anomalies
    if(sumstat@use.anomalies) {
      mdl.val <- mdl.anom
      
    } else { #Calculate the full field by adding in the appropriate climatology
      #Select the appropriate observation climatology
      obs.clim <- obs.clim.l[[m$which.clim]]
      
      #The resolutions of the observational climatology and the modelled anomaly match 
      #automatically, because an earlier step involves the interpolations of both the 
      #model output and observations onto the same analysis grid. This saves lots
      #of messing around with resolution adjustments etc
      
      #Build up the modelled value by combining the observational climatology
      #with the modelled anomaly.
      mdl.val <- obs.clim + mdl.anom
    }
    
    
    #Apply the land mask 
    #TODO: 20180801 I'm not really sure if we need a landmask at all, so lets drop it and 
    #see what happens.
    #masked <- mask(mdl.val,landmask,maskvalue=1)
    
    #Apply the polygon mask    
    masked.r <- mask(mdl.val,this.sp@boundary)
    
    #Set the dates of the raster to be processed to be the same as those in the
    #original source file - these processing steps don't always propigate them
    #correctly
  #  masked <- setZ(masked,getZ(mdl.anom))
    
    # #Finally, drop layers that are completely blank
    # blank.layer <- cellStats(is.na(masked),sum)==ncell(masked)
    # no.blanks <- masked[[which(!blank.layer)]]
    
    #And we're ready. Lets calculate some summary statistics
    res <- eval.sum.stat(ss=sumstat,r=masked.r) 
    
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
  sumstat.res <- bind_rows(res.l) %>% 
              as.tibble() %>%
              add_column(sp.subdomain=this.sp@name,
                         sumstat.name=sumstat@name,
                         sumstat.type=class(sumstat),
                         sumstat.data.type=sumstat@data.type,
                         .before=1)

  #Store results
  save.fname <- gsub(" ","-",sprintf("%s_%s_%s_%s.RData",
                                     this.sp@name,this.cfg$src.type,
                                     this.cfg$src.name,sumstat@name))
  save(sumstat.res,file=file.path(sumstat.dir,save.fname))
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
