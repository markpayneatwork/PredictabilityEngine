#/*##########################################################################*/
#' Create a persistence forecast 
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jul 14 11:34:17 2016
#'
#' Uses the existing observational data to create a persistence forecast. This
#' is done simply by tweaking the existing metadata catalogues
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

#/*======================================================================*/
#  Initialise system
#/*======================================================================*/
cat(sprintf("\n%s\n","Create a Persistence Forecast"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")
load("objects/PredEng_config.RData")

library(lubridate)
library(raster)
library(tibble)
library(dplyr)

#/*======================================================================*/
#  Configuration
#/*======================================================================*/
#Setup spatial configurations
if(pcfg@use.global.ROI) { #only need to use one single global ROI
  this.sps <- list(spatial.domain(name="",boundary=pcfg@global.ROI))
} else { #Working with subdomains
  this.sps <- pcfg@spatial.subdomains
}

#/*======================================================================*/
#  Create (pseudo) metadata
#  The trick here is that we can create a pseudo  persistence forecast
#  by simply tweaking the metadata properly to reuse existing observational
#  data files. We loop over the individual spatial subdomains in a single
#  run.
#/*======================================================================*/

for(this.sp in this.sps) {
  log_msg("Creating pseudo metadata for %s...\n",this.sp@name)
  
  #Working directories
  subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
  base.dir <- define_dir(subdomain.dir,"Persistence",pcfg@Observations@name)

  #Load the monthly anomaly data
  load(file.path(subdomain.dir,"Observations",pcfg@Observations@name,
                 PE.cfg$files$Obs.monthly.anom.metadata))
  
  #Some tweaks
  anom.meta <- mutate(mon.anom.meta,
                      type="Persistence")
  
  #Save metadata
  save(anom.meta,file=file.path(base.dir,"Anomaly_metadata.RData"))
  realmean.meta <- anom.meta
  save(realmean.meta,file=file.path(base.dir,"Realmean_metadata.RData"))
}

# #/*======================================================================*/
#  Complete
#/*======================================================================*/
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
