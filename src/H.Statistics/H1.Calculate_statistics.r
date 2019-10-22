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
# *  We choose to parallelise over datasources, rather than over statistics,
#    which would be an alternative structure. The logic behind this is that
#    not all data sets are available on one machine at the same time, due to
#    storage and practical limitations - however, we can always apply the
#    statistics (I hope). Thus, it makes more sense to parallelise over datasources
#    and then loop over statistics.
#
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
  cfg.no <- 34
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
# Divide work ####
#'========================================================================
#Retrieve configurations
cfg.fname <- file.path(PE.cfg$dirs$job.cfg,"Stats.cfg")
these.cfgs <- get.cfgs(cfg.fname)
this.cfg <- these.cfgs[cfg.no,]
this.src <- configure.src(cfg.fname,cfg.no,pcfg)
this.sp <- configure.sp(cfg.fname,cfg.no,pcfg)
config.summary(pcfg, this.src,this.sp)

if(this.src@type=="Persistence" & !pcfg@average.months & length(pcfg@MOI) >1 &
   any(!purrr::map_lgl(pcfg@statistics,slot,"use.full.field"))){
  stop("Don't know how to handle a persistence forecast for full
       field statistics in presence of multiple months")
}

if(!is.na(this.cfg$sp)) {
  stop("This code hasn't been checked recently using subdomain focused analyses. Check it before proceeding")
}

#'========================================================================
# Setup ####
#'========================================================================
#Directory setup
base.dir <-  get.subdomain.dir(pcfg,this.sp) 
obs.dir <- file.path(base.dir,pcfg@Observations@type,pcfg@Observations@name)
stat.dir <- define_dir(base.dir,PE.cfg$dirs$statistics)

#Setup observational climatology
clim.meta <- readRDS(file.path(obs.dir,PE.cfg$files$Obs.climatology.metadata))
obs.clim.l <- lapply(clim.meta$fname,brick)
names(obs.clim.l) <- sprintf("%02i",month(clim.meta$date))

#Setup landmask 
landmask <- raster(file.path(base.dir,PE.cfg$files$regridded.landmask))

#Result storage
sum.stats.l <- list()

#'========================================================================
# Setup metadata ####
#'========================================================================
#Load metadata
#Remember that the metadata is preselected at the workload partitioning stage
if(file.exists(this.cfg$metadat.path)) {
  # metadat.varname <- load(metadat.path)
  # metadat <- get(metadat.varname)    
  metadat.all <-readRDS(this.cfg$metadat.path)
  
} else {#Fail gracefully
  log_msg(sprintf("Error:Cannot find file %s.",this.cfg$metadat.path))
  stop()
}

#Partition the whole metadata into chunks
#Note that we make these into equally sized chunks based on the number of
#files - this a little bit different to how the original division was performed
#(i.e. based on file size)
metadat <- 
  metadat.all %>%
  mutate(chunk.idx=ceiling(seq(nrow(.))/nrow(.)*this.cfg$n.chunks)) %>%
  filter(chunk.idx == this.cfg$chunk.idx)

#'========================================================================
# Setup spatial subdomains / statistics ####
#'========================================================================
#The statistics can inform the spatial domain that we need to apply - 
# particularly for statistics that work with  fields, instead of singular
#values, we want to process the entire global domain, rather than just a single
#local spatial domain. Hence, we need to ensure that there is a match between
#these two aspects
sp.stat.all <- 
  expand.grid(sp=c(global.ROI(pcfg),
                   pcfg@spatial.subdomains),
              stat=pcfg@statistics) %>%
  mutate(sp.name=map_chr(sp,slot,"name"),
         is.global.sp=sp.name==PE.cfg$misc$global.sp.name,
         stat.name=map_chr(stat,slot,"name"),
         use.stat.globally=map_lgl(stat,slot,"use.globally"))

#Now restrict, by ensuring that there is agreement between the stats to be 
#calculated and the spatial subdomain
sp.stat.sel <-
  sp.stat.all %>%
  filter(is.global.sp==use.stat.globally)

#'========================================================================
# Apply statistics ####
#'========================================================================
# Loop over statistics ------------------------------------------------------------
res.l <- vector("list",nrow(sp.stat.sel))
for(j in seq(nrow(sp.stat.sel))) {
  this.stat <- sp.stat.sel[j,"stat"][[1]]
  this.sp <- sp.stat.sel[j,"sp"][[1]]
  log_msg("Processing '%s' statistic for '%s' subdomain...\n",
          this.stat@name,this.sp@name)
  
  
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
      mdl.dat <- brick(obs.clim + mdl.anom)
      mdl.dat <- setZ(mdl.dat,getZ(mdl.anom))  #...and correcting the Z values
      
    } else { #Use the anomaly
      mdl.dat <- mdl.anom
    }
    
    #Apply the masks to data
    masked.dat <- mask(mdl.dat,combined.mask,maskvalue=1)
    
    #And we're ready. Lets calculate some summary statistics
    res <- eval.stat(st=this.stat,dat=masked.dat) 
    
    #Add in the metadata and store the results
    #Doing the bind diretly like this is ok when we are dealing with
    #rasterLayer fragments, but we will need to be caseful when dealing with 
    #bricks, for example
    file.res.l[[i]] <- bind_cols(slice(m,rep(1,nrow(res))),res)
    
  }
  
  Sys.sleep(0.1)
  print(pb$stop())
  log_msg("\n")
  
  #Combine results with metadata
  
  
  #Tidy up results a bit more
  res.l[[j]] <- 
    bind_rows(file.res.l) %>% 
    as_tibble() %>%
    add_column(sp.subdomain=this.sp@name,
               stat.name=this.stat@name,
               .before=1) %>%
    dplyr::select(-fname,-which.clim,-chunk.idx)
  
}

#Store results
bind_rows(res.l) %>%
saveRDS(file=file.path(stat.dir,sprintf("Stats_chunk_%04i.rds",this.cfg$cfg.id)))


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
