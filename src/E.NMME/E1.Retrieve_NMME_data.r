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
#Helper functions, externals and libraries
load("objects/common_elements.RData")

library(reshape2)
library(stringr)
library(ncdf4)
library(raster)

# ========================================================================
# Configuration
# ========================================================================
base.dir <- "data/NMME"
cfg.dir <- file.path(base.dir,"1.config")
download.dir <- file.path(base.dir,"2.downloads")
mean.dir  <- file.path(base.dir,"3.means")

options("run.level"= 0)  #0 complete fresh run

# ========================================================================
# Download data
# ========================================================================
#Import configurations
cfg.fnames <- dir(cfg.dir,pattern = "txt$",full.names = TRUE)

#Define spatial ROI
ROI.str <- do.call(sprintf,c("-d X,%.2f,%.2f -d Y,%.2f,%.2f",
                             as.list(as.vector(extract.ROI))))
#Loop over configurations
for(f in cfg.fnames) {
  #Setup prefixes
  cfg.prefix <- gsub(".txt$","",basename(f))
  file.stem <- sprintf("%s_%s",proj.prefix,cfg.prefix)
  
  #Download
  urls <- data.frame(url=readLines(f))
  urls$download.fname <- file.path(download.dir,
                                   sprintf("%s_%02i.nc",file.stem,seq(nrow(urls))))
  for(u in seq(nrow(urls))) {
    log_msg("\nDownloading %i of %i, %s...\n",u, length(urls),cfg.prefix)

    #Download
    download.fname <- urls$download.fname[u]
    download.cmd <- paste("ncks -O --netcdf4 -D1",
                          ROI.str,urls$url[u],download.fname)
    run_if(1,download.cmd)
    
    #Set record dimension
    record.cmd <- paste("ncks --mk_rec_dmn S -O",download.fname,download.fname)
    run_if(2,record.cmd)
  }
  
  #Concatenate (if necessary)
  build.fname <- file.path(download.dir,sprintf("%s.nc",file.stem))
  build.cmd <- paste("ncrcat -D 1 -O -o",build.fname,
                     paste(urls$download.fname, collapse=" "))
  run_if(3,build.cmd)
  unlink(urls$download.fname)  #Remove individual files
  
  #Set _FillValue
  missval.cmd <- paste("ncrename -a .missing_value,_FillValue",build.fname)
  run_if(4,missval.cmd)
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
