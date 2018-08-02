#/*##########################################################################*/
#' Retrieve NMME data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Mon May 23 10:45:27 2016
#'
#' Retrieves a subset of the NMME data directly from the servers using OpenDAP
#' via NCKS. The approach taken here is to do most of the subsetting locally -
#' while this is more intensive on storage, in practice it also avoids having
#' to make endless OpenDAP calls, which are really rather slow.
#' 
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
cat(sprintf("\n%s\n","Retrieve NMME data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(readr)
library(dplyr)
library(tibble)
library(ncdf4)
load("objects/PredEng_config.RData")
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
  src.no <- 2
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent(TRUE)
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--ovewrite")
} else {
  #Taking inputs from the system environment
  src.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(src.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything and tell us all about it
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent(FALSE)
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
}

#Other configurations
set.nco.defaults("--overwrite")

#Extract configurations
if(pcfg@use.global.ROI) { #only need to use one single global ROI
  this.src <- pcfg@NMME.models[[src.no]]
  this.sp  <- spatial.subdomain(pcfg@global.ROI,name="")  
} else { #Working with subdomains
  cfgs <- expand.grid(src=names(pcfg@NMME.models),
                      sp=names(pcfg@spatial.subdomains))
  this.src <- pcfg@NMME.models[[cfgs$src[src.no]]]
  this.sp <- pcfg@spatial.subdomains[[cfgs$sp[src.no]]]
}


#Configure directories
base.dir <- define_dir(pcfg@scratch.dir,this.sp@name,"NMME",this.src@name)
download.dir <- define_dir(base.dir,"0.data")

set.debug.level(0)  #0 complete fresh run. 1 downloads missing files

# ========================================================================
# Setup
# ========================================================================
#Import metadata
load(file.path(pcfg@scratch.dir,"NMME_archive_metadata.RData"))
this.meta <- subset(meta,Model==this.src@name)

#Define spatial ROI string (for input into NCKS)
#NMME works on the basis of a 0 to 360 grid, so we need to account for that
#However, NCKS can take care of the differences
extract.ROI <- extent(this.sp)
extract.ROI[1:2] <- Pacific.centered(extract.ROI[1:2])
ROI.str <- do.call(sprintf,c("-d X,%.2f,%.2f -d Y,%.2f,%.2f",
                             as.list(as.vector(extract.ROI))))

#Download datetime
download.datetime <- format(Sys.time(),"%Y%m%d_%H%M%S")

#Now, to allow for easy updating of data sets, we can first check what we have 
#already available in the archive
#TODO
downloaded.fnames <- dir(download.dir,full.names = TRUE)
downloaded.dates.l <- lapply(downloaded.fnames,function(f) {
                      ncid <- nc_open(f)
                      dates <- ncid$dim$S$vals
                      nc_close(ncid)
                      return(dates)
                      })
downloaded.dates <- unlist(downloaded.dates.l)
dates.to.download <- lapply(SLM["S",this.meta$mdl.str],function(x) {
                      x[!(x %in% downloaded.dates)]})

#'========================================================================
# Download data ####
#'========================================================================
#Loop over model data sets
for(i in seq(nrow(this.meta))) {
  #Setup download details
  mdl.cfg <- this.meta[i,]
  mdl.id <- mdl.cfg$mdl.str
  mdl.dates.to.download <- dates.to.download[[mdl.id]]

  if(length(mdl.dates.to.download)!=0) {
    log_msg("Downloading %i of %i start dates from %s...\n",length(mdl.dates.to.download),mdl.cfg$n.starts,mdl.id)
    #Setup time range
    download.rng <- range(mdl.dates.to.download)
    timeROI.str <- sprintf("-d S,%.2f,%.2f",download.rng[1],download.rng[2])

    #Setup for download
    download.fname <- sprintf("%s_S%03i-S%03i.nc",mdl.cfg$Model,download.rng[1],download.rng[2])
    download.full.path <- file.path(download.dir,download.fname)
    download.cmd <- ncks("--netcdf4 -D1",
                         ROI.str,timeROI.str,
                         mdl.cfg$URL,
                         download.full.path)
    
    #Download missing file
    condexec(1,download.cmd,silent=TRUE)
    
    #Set _FillValue
    missval.cmd <- ncrename("-a .missing_value,_FillValue",download.full.path)
    condexec(2,missval.cmd,silent=TRUE)
    
    #Convert X axis to [-180,180]
    #TODO
    # ncid <- nc_open(download.full.path,write=TRUE)
    # nc_redef(ncid)
    # X.dim <- ncid$dim$X
    # X.dim$vals <- Pacific.centered(X.dim$vals) 
    # nc_create()
    # correct.lon <- ncap2("--overwrite",
    #                      '-s "where (X>180) X=X-360;"',
    #                      download.full.path,download.full.path)
    # condexec(3,correct.lon)
    
  } else {
    log_msg("Skipping download from %s (all dates present).\n",mdl.id)
  } 
}

# ========================================================================
# Complete
# ========================================================================
#+ results='asis'
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
