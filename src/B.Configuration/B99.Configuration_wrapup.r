#/*##########################################################################*/
#' Configuration Wrapup
#' ==========================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' http://www.staff.dtu.dk/mpay
#'
#' Tue Jul 12 23:05:44 2016
#'
#' Handles the post-processing of a configuration
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

#'========================================================================
# Initialise system
#'========================================================================

#'========================================================================
# Output ####
#'========================================================================
#Write output files 
log_msg("Writing output files...\n")

base.dir <- define_dir(pcfg@scratch.dir)
if(pcfg@use.global.ROI){
  log_msg("Writing Global Outputs...\n")
  #Write CDO grid descriptors
  this.ROI <- extend(pcfg@global.ROI,PE.cfg$ROI.extraction.buffer)
  analysis.grid.fname <- file.path(base.dir,PE.cfg$files$analysis.grid)
  griddes.txt <- griddes(this.ROI,res=pcfg@global.res)
  writeLines(griddes.txt,analysis.grid.fname)
  
  #Write regridded landmask
  regrid.landmask <- file.path(pcfg@scratch.dir,PE.cfg$files$regridded.landmask)
  exec(landmask.cmd <- cdo("--silent -f nc",
                           csl(" remapnn", pcfg@analysis.grid.fname),
                           pcfg@landmask,
                           regrid.landmask))

} else { #Loop over spatial subdomains
  for(sp in pcfg@spatial.subdomains){
    log_msg("Writing outputs for %s...\n",sp@name)
    #Write CDO grid descriptors
    sp.dir <- define_dir(base.dir,sp@name)
    this.ROI <- extend(extent(sp),PE.cfg$ROI.extraction.buffer)
    griddes.txt <- griddes(this.ROI,res=pcfg@global.res) 
    analysis.grid.fname <- file.path(sp.dir,PE.cfg$files$analysis.grid)
    writeLines(griddes.txt,analysis.grid.fname)
    
    #Write regridded landmask
    regrid.landmask <- file.path(sp.dir,PE.cfg$files$regridded.landmask)
    exec(landmask.cmd <- cdo("--silent -f nc",
                             csl("remapnn", analysis.grid.fname),
                             pcfg@landmask,
                             regrid.landmask))
  }  
}

#Output
save(pcfg,file="objects/configuration.RData")
save(pcfg,file=file.path(pcfg@scratch.dir,"configuration.RData"))

#'========================================================================
# HPC  Configuration ####
#'========================================================================
#Extract configurations
require(tidyverse)

project.cfg <- define_dir(pcfg@scratch.dir,"Job_configuration")
unlink(PE.cfg$dirs$cfg)
cfg.dir <- file.symlink(file.path(getwd(),project.cfg),PE.cfg$dirs$cfg)
cfgs <- partition.workload(file.path(cfg.dir,"NMME.cfg"),pcfg,"NMME")
cfgs <- partition.workload(file.path(cfg.dir,"NMME_Ensmean.cfg"),pcfg,NA)
cfgs <- partition.workload(file.path(cfg.dir,"Decadal.cfg"),pcfg,"Decadal")
cfgs <- partition.workload(file.path(cfg.dir,"Decadal_Ensmean.cfg"),pcfg,NA)
cfgs <- partition.workload(file.path(cfg.dir,"Observations.cfg"),pcfg,"Observations")

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
