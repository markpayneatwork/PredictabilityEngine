#/*##########################################################################*/
#' Explode downloaded data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Tue May 15 13:29:02 2018
#'
#' NMME data is inherently 5D when it is downloaded (lat, lon, start date, lead,
#' realization). To get it into a format where it can be picked up by the rest
#' of the codebase, we need to "explode" it so that the pieces are 2D in nature.
#' We do this based primarily on the ncks tool. Ideally, we would also like to
#' end with files that are compatible with CDO, but we shall see if that is possible
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

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Explode Downloaded Data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(reshape2)
library(stringr)
library(ncdf4)
library(raster)
library(lubridate)
library(tibble)
library(dplyr)
load("objects/setup.RData")
load("objects/configuration.RData")

#'========================================================================
# Configuration ####
#'========================================================================
NMME.dat.dir <- file.path(pcfg@scratch.dir,"NMME")
download.dir <- define_dir(NMME.dat.dir,"0.data")
fragment.dir <- define_dir(NMME.dat.dir,"1.fragments")

set.debug.level(0)  #0 complete fresh run
set.nco.defaults("--overwrite --netcdf4")
set.condexec.silent()

#'========================================================================
# Setup ####
#'========================================================================
#Import metadata
load(file.path(NMME.dat.dir,"NMME_archive_metadata.RData"))

#'========================================================================
# Explode data ####
#'========================================================================
#Loop over model data sets
for(i in seq(nrow(meta))) {
  mdl.cfg <- meta[i,]
  mdl.id <- mdl.cfg$mdl.str
  GCM.obj <- pcfg@NMME.models[[mdl.cfg$Model]]
  download.fname <- file.path(download.dir,
                              sprintf("NMME_%s.nc",mdl.id))

  #Figure out what's available, and what we actually want to extract
  all.SL <- expand.grid(S.idx=seq(SLM[["S",mdl.id]]),
                        L.idx=seq(SLM[["L",mdl.id]])) %>%
    as.tibble() %>%
    mutate(S.val=SLM[["S",mdl.id]][S.idx],
           L.val=SLM[["L",mdl.id]][L.idx],
           start.date=epoch.start+months(S.val),
           forecast.date=start.date+months(floor(L.val)),
           forecast.month=month(forecast.date))
  
  #Now restrict to the relevant months
  sel.SL <- subset(all.SL,forecast.month %in% pcfg@MOI)

  #Select realisations to explode  
  if(any(GCM.obj@realizations==0)){
    sel.M <- SLM[["M",mdl.id]]
  } else {
    sel.M <- GCM.obj@realizations
  }

  #Now comes the mega loop, where we loop over the start dates and members as well!
  log_msg("Exploding %s, model  %02i of %02i...\n",mdl.id,i,nrow(meta))
  pb <- progress_estimated(nrow(sel.SL))
  for(j in seq(nrow(sel.SL))) {
    pb$tick()$print()
    sel.2D <- sel.SL[j,]
    for(m in sel.M) {
      #Setup for explode
      fragment.fname <- sprintf("NMME_%s_S%s_L%02.1f_r%03i.nc",
                                mdl.id,
                                format(sel.2D$start.date,"%Y%m%d"),
                                sel.2D$L.val,
                                m)
      fragment.full.path <- file.path(fragment.dir,fragment.fname)
      
      SLM.ROI.str <- sprintf("-d S,%i -d L,%i -d M,%i",
                             sel.2D$S.idx,
                             sel.2D$L.idx,
                             m)
      explode.cmd <- ncks("--fortran",   #Use indexing starting at 1, like in R
                            SLM.ROI.str,
                            download.fname,
                            fragment.full.path)
      condexec(1,explode.cmd)
      
      #Now apply ncwa to reduce the dimensionality so that we end
      #up with something compatable with CDO
      #We choose to drop the realization dimnension, to end with 
      #a 4D variable
      ncwa.cmd <- ncwa(sprintf("-a %s,M",GCM.obj@var),
                       fragment.full.path,fragment.full.path)
      condexec(1,ncwa.cmd)
    }
  }
  Sys.sleep(0.1)
  print(pb$stop())
  log_msg("\n")
}



#'========================================================================
# Complete 
#'========================================================================
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
