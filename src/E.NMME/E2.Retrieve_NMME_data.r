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
load("objects/setup.RData")
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
NMME.dir <- define_dir(pcfg@scratch.dir,"NMME")
download.dir <- define_dir(NMME.dir,"0.data")

set.debug.level(0)  #0 complete fresh run. 1 downloads missing files

# ========================================================================
# Setup
# ========================================================================
#Import metadata
load(file.path(NMME.dir,"NMME_archive_metadata.RData"))

#Define spatial ROI string (for input into NCKS)
#NMME works on the basis of a 0 to 360 grid, so we need to account for that
#However, NCKS can take care of the differences
extract.ROI <- ifelse(pcfg@ROI[1:4]<0,pcfg@ROI[1:4]+360,pcfg@ROI[1:4])
ROI.str <- do.call(sprintf,c("-d X,%.2f,%.2f -d Y,%.2f,%.2f",
                             as.list(extract.ROI)))

# ========================================================================
# Download data
# ========================================================================
#Loop over model data sets
for(i in seq(nrow(meta))) {
  mdl.cfg <- meta[i,]
  mdl.id <- mdl.cfg$mdl.str
  log_msg("Downloading %s...\n",mdl.id)
  
  #Setup for download
  download.fname <- sprintf("%s.nc",mdl.id)
  download.full.path <- file.path(download.dir,download.fname)
  download.cmd <- ncks("--netcdf4 -D1",
                        ROI.str,
                        mdl.cfg$URL,
                        download.full.path)
  
  #Handle the case of files already present
  if(file.exists(download.full.path)) {
    if(get.debug.level()<=0) {
      #Delete it
      unlink(download.full.path)
      #Then download
      condexec(0,download.cmd,silent=TRUE)
    } #If running at a higher debug level, then don't re-download
  } else {#Download missing file
    condexec(1,download.cmd,silent=TRUE)
  }
  
  #Set _FillValue
  missval.cmd <- ncrename("-a .missing_value,_FillValue",download.full.path)
  condexec(2,missval.cmd,silent=TRUE)
  
  #Convert X axis to [-180,180]
  correct.lon <- ncap2("--overwrite",
                     '-s "where (X>180) X=X-360;"',
                     download.full.path,download.full.path)
  condexec(3,correct.lon)
  
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
