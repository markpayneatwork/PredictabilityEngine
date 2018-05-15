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
#' Retrieves a subset of the NMME data directly from the servers using OpenDAP. 
#' Averaging across ensembles is done on the fly, which may or may not be
#' an advantage, depending on the application, and what level of uncertainty
#' one is interested in.
#
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
load("objects/setup.RData")
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
NMME.config.fname <- file.path(datasrc.dir,"NMME_urls.csv")
base.dir <- pcfg@scratch.dir

set.debug.level(0)  #0 complete fresh run

# ========================================================================
# Download data
# ========================================================================
#Import configurations
NMME.cfg <- read_csv2(NMME.config.fname)

#Define spatial ROI string (for input into NCKS)
ROI.str <- do.call(sprintf,c("-d X,%.2f,%.2f -d Y,%.2f,%.2f",
                             as.list(pcfg@ROI[1:4])))

#Some modifications
NMME.cfg <- mutate(NMME.cfg,
                   file.stem=sprintf("NMME_%s_%s",Model,type))

#Loop over model configurations
for(i in seq(nrow(NMME.cfg))) {
  log_msg("Downloading %s...\n",NMME.cfg$file.stem[i])

  #Setup for download
  download.dir <- define_dir(pcfg@scratch.dir,"NMME_data")
  download.fname <- file.path(download.dir,sprintf("%s.nc",NMME.cfg$file.stem[i]))
  
  #Download
  download.cmd <- paste("ncks -O --netcdf4 -D1",
                        ROI.str,
                        NMME.cfg$URL[i],
                        download.fname)
  condexec(1,download.cmd)
  
  # #Set record dimension
  # record.cmd <- paste("ncks --mk_rec_dmn S -O",download.fname,download.fname)
  # run_if(2,record.cmd)

  #Set _FillValue
  missval.cmd <- paste("ncrename -a .missing_value,_FillValue",download.fname)
  condexec(4,missval.cmd)
}

save(NMME.cfg,file=file.path(download.dir,"NMME_metadata.RData"))
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
