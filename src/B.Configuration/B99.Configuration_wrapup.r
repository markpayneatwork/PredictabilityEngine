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
# Validity checks ####
#'========================================================================
validObject(pcfg)

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
  landmask.cmd <- cdo("--silent -f nc",
                           csl(" remapnn", analysis.grid.fname),
                           pcfg@landmask,
                           regrid.landmask)

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
cfg.fname <- file.path(pcfg@scratch.dir,PE.cfg$config.fname)
cfg.linked <- PE.cfg$config.path
saveRDS(pcfg,file=cfg.fname)
if(file.exists(cfg.linked)) {
 file.remove(cfg.linked)
}
file.symlink(file.path(getwd(),cfg.fname),"objects")

#'========================================================================
# HPC  Configuration ####
#'========================================================================
#Setup soft linking
project.cfg <- define_dir(pcfg@scratch.dir,basename(PE.cfg$dirs$job.cfg))
unlink(PE.cfg$dirs$job.cfg) 
file.symlink(file.path(getwd(),project.cfg),PE.cfg$dirs$job.cfg) 

#Need a TODO directory as well
TODO.dir <- define_dir(PE.cfg$dirs$job.cfg,"TODO")

#Write configurations
cfgs <- partition.workload(pcfg,"NMME","Sources")
cfgs <- partition.workload(pcfg,"NMME","Ensmean")
cfgs <- partition.workload(pcfg,"Decadal","Sources")
cfgs <- partition.workload(pcfg,"Decadal","Chunks")
cfgs <- partition.workload(pcfg,"Decadal","Ensmean")
cfgs <- partition.workload(pcfg,"CMIP5","Sources")
cfgs <- partition.workload(pcfg,"CMIP5","Ensmean")
cfgs <- partition.workload(pcfg,"Observations")
cfgs <- partition.workload(pcfg,"Stats",space.partition=TRUE)

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
