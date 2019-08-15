#/*##########################################################################*/
#' E1. Retrieve NMME data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Mon May 23 10:45:27 2016
#'
#' Retrieves all of the NMME data directly from the servers using OpenDAP
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
#   - Note that this script is intended to run independently from the rest of
#     the PredEng workflow, and therefore does not require (or use) a 
#     configuration object
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","E1. Retrieve NMME data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tidyverse)
library(ncdf4)
library(parallel)

# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.no <- 1
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--ovewrite")
  options("mc.cores"=1)  
} else {
  #Taking inputs from the system environment
  cfg.no <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.no=="") stop("Cannot find LSB_JOBINDEX")
  #Do everything and tell us all about it
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
  options("mc.cores"= as.numeric(Sys.getenv("LSB_MAX_NUM_PROCESSORS")))
  options("mc.cores"=1)  
}

#Other configurations
set.nco.defaults("--overwrite")

#Configure directories
base.dir <- define_dir(PE.cfg$dirs$datasrc,"NMME")

#'========================================================================
# Setup ####
#'========================================================================
#Import configurations from config object
NMME.cfg <- read_csv2(file.path(PE.cfg$dirs$datasrc,"NMME","NMME_SST_urls.csv"),
                      col_types = cols())

# ========================================================================
# Retrieve meta data
# ========================================================================
#Choose source to work with
this.src <-NMME.cfg[cfg.no,] %>% mutate(mdl.str=sprintf("%s_%s",Model,type))

#open file via OpenDAP
log_msg("Now retrieving metadata from %s...\n",this.src$mdl.str)
ncid <- nc_open(this.src$URL)
#Extract meta data
meta <- tibble(forecast_period=ncid$dim$L$len,
               ensemble_members=ncid$dim$M$len,
               first.start=min(ncid$dim$S$vals),
               last.start=max(ncid$dim$S$vals),
               n.starts=ncid$dim$S$len,
               n.leads=ncid$dim$L$len,
               start_units=ncid$dim$S$units) %>%
  bind_cols(this.src)


#List of starts and leads
SLM <- list(S=ncid$dim$S$vals,
            L=ncid$dim$L$vals,
            M=ncid$dim$M$vals)

#Close file
nc_close(ncid)

#Correct dates etc
meta$first.start.date <-  PE.cfg$misc$NMME.epoch.start  + months(meta$first.start)
meta$last.start.date  <-  PE.cfg$misc$NMME.epoch.start  + months(meta$last.start)

# #Save results
# saveRDS(meta,file=file.path(base.dir,sprintf("%s_metadata.rds",this.src$mdl.str)))
# saveRDS(SLM,file=file.path(base.dir,sprintf("%s_SLM.rds",this.src$mdl.str)))

# ========================================================================
# Setup
# ========================================================================
#Download datetime
download.datetime <- format(Sys.time(),"%Y%m%d_%H%M%S")
download.dir <- define_dir(base.dir,this.src$Model)

#Remove any incomplete files (ie ending with .tmp)
file.remove(dir(download.dir,full.names=TRUE,pattern=".tmp$"))

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
starts.to.download <- SLM$S[!(SLM$S %in% downloaded.dates)]

#'========================================================================
# Download data ####
#'========================================================================
#Setup download details
to.download <- tibble(starts=starts.to.download,
                      idxs=which(SLM$S%in% starts))

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

#However!! We can save ourselves a lot of pain at a later stage by converting from a
#Pacific centered to a Greenwich centered orientation right now. We take advantage of the
#multislab functionality of nco to do this
# ROI.str <- "--msa_user_order -d X,181.00,360.00 -d X,0.00,180.00"
ROI.str <- ""

if(nrow(to.download)!=0){
  
  #Download by chunks
  download.fn <- function(i) {   
    log_msg("Downloading chunk %i of %i from %s (S%i) ...\n",i,nrow(to.download),meta$mdl.str,
            to.download$starts[i])
    
    #Setup time range
    timeROI.str <- sprintf("-F -d S,%i",to.download$idxs[i])  #Remember Fortran indexing..
    
    #Setup for download
    download.fname <- sprintf("%s_S%03i.nc",meta$Model,to.download$starts[i])
    download.full.path <- file.path(download.dir,download.fname)
    download.cmd <- ncks("--netcdf4 --deflate 1",  #Deflation
                         ROI.str,
                         timeROI.str,
                         meta$URL,
                         download.full.path)

    #Set _FillValue
    # missval.cmd <- ncrename("-a .missing_value,_FillValue",download.full.path)
    # condexec(2,missval.cmd,silent=TRUE)
    # 
    # # Convert X axis to [-180,180]
    # # Note that this is not necessary when we are using cdo later to do the remapping
    # ncid <- nc_open(download.full.path,write=TRUE)
    # new.X <- Greenwich.centered(ncid$dim$X$vals)
    # ncvar_put(ncid,"X",vals=new.X)
    # nc_close(ncid)
    
  }
  
  mclapply(seq(nrow(to.download)),download.fn)    
  
} else {
  log_msg("Skipping download from %s (all dates present).\n",meta$mdl.str)
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
