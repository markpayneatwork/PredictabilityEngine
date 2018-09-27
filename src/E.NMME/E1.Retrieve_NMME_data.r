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
#' Retrieves allof the NMME data directly from the servers using OpenDAP
#' via NCKS. The approach taken here is to do most of the subsetting locally after
#' we have downloaded it. While this is more intensive on storage, it solves a lot
#' of problems around downloading
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
library(parallel)
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.no <- 1
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent(TRUE)
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--ovewrite")
} else {
  #Taking inputs from the system environment
  cfg.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(cfg.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything and tell us all about it
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent(FALSE)
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
  n.cores <- 1
}
do.parallel <- FALSE

#Other configurations
set.nco.defaults("--overwrite")

#Retrieve configurations
this.src <- get.this.src(file.path(PE.cfg$dirs$cfg,"NMME.cfg"),cfg.no,pcfg)

#Configure directories
base.dir <- define_dir(PE.cfg$dirs$datasrc,"NMME")
download.dir <- define_dir(base.dir,this.src@name)

config.summary(pcfg,this.src)

# ========================================================================
# Retrieve meta data
# ========================================================================
#Import configurations from config object
NMME.cfg <-tibble(Model=this.src@name,
                  type=names(this.src@source),
                  URL=this.src@source) %>%
  mutate(mdl.str=sprintf("%s_%s",Model,type))

#Data storage
meta.db.l <- list()
SLM.l <- list()
#Loop over files
for(i in seq(nrow(NMME.cfg))) {
  mdl <- NMME.cfg[i,]
  log_msg("Now retrieving metadata from %s...\n",mdl$mdl.str)
  #open file via OpenDAP
  ncid <- nc_open(mdl$URL)
  #Extract meta data
  res <- tibble(forecast_period=ncid$dim$L$len,
                ensemble_members=ncid$dim$M$len,
                first.start=min(ncid$dim$S$vals),
                last.start=max(ncid$dim$S$vals),
                n.starts=ncid$dim$S$len,
                n.leads=ncid$dim$L$len,
                start_units=ncid$dim$S$units) %>%
    bind_cols(mdl)
  meta.db.l[[mdl$mdl.str]] <- res
  
  #List of starts and leads
  SLM.l[[mdl$mdl.str]]$S <- ncid$dim$S$vals
  SLM.l[[mdl$mdl.str]]$L <- ncid$dim$L$vals
  SLM.l[[mdl$mdl.str]]$M <- ncid$dim$M$vals
  
  #Close file
  nc_close(ncid)
}

#Collate meta data
meta <- bind_rows(meta.db.l)

#Collate list of starts and leads
SLM <- do.call(cbind,SLM.l)

#Correct dates etc
meta$first.start.date <-  PE.cfg$NMME.epoch.start  + months(meta$first.start)
meta$last.start.date  <-  PE.cfg$NMME.epoch.start  + months(meta$last.start)

#Save results
save(meta,SLM,file=file.path(base.dir,sprintf("%s_metadata.RData",this.src@name)))

# ========================================================================
# Setup
# ========================================================================
#Download datetime
download.datetime <- format(Sys.time(),"%Y%m%d_%H%M%S")

#Now, to allow for easy updating of data sets, we can first check what we have 
#already available in the archive
downloaded.fnames <- dir(download.dir,full.names = TRUE)
downloaded.dates.l <- lapply(downloaded.fnames,function(f) {
                      ncid <- nc_open(f)
                      dates <- ncid$dim$S$vals
                      nc_close(ncid)
                      return(dates)
                      })
downloaded.dates <- unlist(downloaded.dates.l)
starts.to.download <- lapply(SLM["S",meta$mdl.str],function(x) {
                      x[!(x %in% downloaded.dates)]})
names(starts.to.download) <- meta$mdl.str

#'========================================================================
# Download data ####
#'========================================================================
#Loop over model data sets
for(i in seq(nrow(meta))) {
  #Setup download details
  mdl.cfg <- meta[i,]
  mdl.id <- mdl.cfg$mdl.str
  to.download <- tibble(starts=starts.to.download[[mdl.id]],
                        idxs=which(SLM[["S",mdl.id]] %in% starts))
  
  #We are now downloading the entire dataset, so we don't need to worry
  #about spatial subsetting anymore
  
  # #Define spatial ROI string (for input into NCKS)
  # #NMME works on the basis of a 0 to 360 grid, so we need to account for that
  # #We do so by taking advantage of the multislab capabilities in NCO
  # #See here: http://nco.sourceforge.net/nco.html#mlt
  # extract.ROI <- as.vector(extent(this.sp))
  # if(all(extract.ROI[1:2]<0)) {
  #   #Just need to adjust coordinates
  #   extract.ROI[1:2] <- Pacific.centered(extract.ROI[1:2])
  #   ROI.str <- do.call(sprintf,c("-d X,%.2f,%.2f -d Y,%.2f,%.2f",
  #                                as.list(extract.ROI)))
  # } else if(identical(extract.ROI[1:2],c(-180,180))) { 
  #   #Download the whole lot (in the X-direction anyway)
  #   ROI.str <- do.call(sprintf,c("-d Y,%.2f,%.2f",
  #                                as.list(extract.ROI[3:4])))
  # 
  #   } else if(any(extract.ROI[1:2]<0)) { 
  #     stop("don't know how to handle crossing of Greenwich properly")
  #   # #Need to make multi slabs, as we are crossing the wrapping point at Greenwich
  #   # slab.1 <- c(0,max(extract.ROI[1:2]))
  #   # slab.2 <- c(Pacific.centered(min(extract.ROI[1:2])),360)
  #   # ROI.str <- do.call(sprintf,c("-d X,%.2f,%.2f -d X,%.2f,%.2f -d Y,%.2f,%.2f",
  #   #                              as.list(c(slab.1,slab.2,extract.ROI[3:4]))))
  #   
  # } else {
  #   ROI.str <- do.call(sprintf,c("-d X,%.2f,%.2f -d Y,%.2f,%.2f",
  #                                as.list(extract.ROI)))
  # }
  # 
  ROI.str <- ""
  
  if(nrow(to.download)!=0){
    
    #Download by chunks
    download.fn <- function(i) {   
      log_msg("Downloading chunk %i of %i from %s (S%i) ...\n",i,nrow(to.download),mdl.id,to.download$starts[i])
      
      #Setup time range
      timeROI.str <- sprintf("-F -d S,%i",to.download$idxs[i])  #Remember Fortran indexing..
      
      #Setup for download
      download.fname <- sprintf("%s_S%03i.nc",mdl.cfg$Model,to.download$starts[i])
      download.full.path <- file.path(download.dir,download.fname)
      download.cmd <- ncks("--netcdf4 --deflate 1",  #Deflation
                           ROI.str,
                           timeROI.str,
                           mdl.cfg$URL,
                           download.full.path)
      
      #Download missing file
      condexec(1,download.cmd,silent=TRUE)
      
      #Set _FillValue
      # missval.cmd <- ncrename("-a .missing_value,_FillValue",download.full.path)
      # condexec(2,missval.cmd,silent=TRUE)
      # 
      #Convert X axis to [-180,180]
      #Note that this is not necessary when we are using cdo later to do the remapping
      #(and avoiding it saves a lot of )
      # ncid <- nc_open(download.full.path,write=TRUE)
      # nc_redef(ncid)
      # X.dim <- ncid$dim$X
      # X.dim$vals <- Pacific.centered(X.dim$vals) 
      # nc_create()
      # correct.lon <- ncap2("--overwrite",
      #                      '-s "where (X>180) X=X-360;"',
      #                      download.full.path,download.full.path)
      # condexec(3,correct.lon)
      
    }
    
    if(do.parallel) {
      #Setup cluster
      log_msg("Setting up cluster..\n")
      cl <- makeCluster(n.cores,type="FORK")
      parLapplyLB(cl,seq(nrow(to.download)),download.fn)
      stopCluster(cl)  
    } else {
      for(i in seq(nrow(to.download))) {
        download.fn(i)
      }
    }
    
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
