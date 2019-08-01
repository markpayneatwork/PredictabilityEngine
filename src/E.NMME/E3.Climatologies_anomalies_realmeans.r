#'##########################################################################
# E3.Climatologies, Anomalies and Realization Means
#'==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed May 16 08:37:49 2018
# 
# Calculates the Climatologies, Anomalies and Realization Means of the NMME 
# forecast data, based on the exploded fragments
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute" 
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
#'##########################################################################

#'==========================================================================
# Initialise system ####
#'==========================================================================
cat(sprintf("\n%s\n","E3.Climatologies, Anomalies and Realization Means"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tidyverse)
library(pbapply)
pcfg <- readRDS(PE.cfg$config.path)

#'==========================================================================
# Configure ####
#'==========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.no <- 1
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--ovewrite")
  options("mc.cores"=7)  
  
} else {
  #Taking inputs from the system environment
  cfg.no <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.no=="") stop("Cannot find LSB_JOBINDEX")
  #Do everything and tell us all about it
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
  options("mc.cores"= as.numeric(Sys.getenv("LSB_MAX_NUM_PROCESSORS"))-1)
  options("mc.cores"=1)  
  
}

#Other configurations
set.nco.defaults("--overwrite")

#Retrieve configurations
cfg.fname <- file.path(PE.cfg$dirs$job.cfg,"NMME_by_sources.cfg")
these.cfgs <- get.this.cfgs(cfg.fname)
this.sp <- get.this.sp(cfg.fname,cfg.no,pcfg)
this.src <- get.this.src(cfg.fname,cfg.no,pcfg)

#Configure directories
subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
base.dir <- define_dir(subdomain.dir,"NMME",this.src@name)
fragstack.dir <- define_dir(base.dir,"1.fragstacks")
lead.clim.dir <- define_dir(base.dir,"2.lead.clims")
anom.dir <- define_dir(base.dir,"A.anoms")
realmean.dir <- define_dir(base.dir,"B.realmean")
analysis.grid.fname <- file.path(subdomain.dir,PE.cfg$files$analysis.grid)
remapping.wts.fname <- file.path(base.dir,PE.cfg$files$remapping.wts)

config.summary(pcfg,cfg.no,this.src,this.sp)

#'==========================================================================
# Setup ####
#'==========================================================================
#Load fragstack metadata
fragstack.meta <- readRDS(file.path(base.dir,PE.cfg$files$fragstack.meta))

#Modify meta data to include climatology and anomaly filenames
anom.meta <- mutate(fragstack.meta,
                    fragstack.fname=fname, #fname is now applied to anom files
                    start.month=month(start.date),
                    forecast.year=year(date),
                    clim.fname=file.path(lead.clim.dir,
                                         sprintf("%s_%s_S%02i_L%s_clim.nc",name,this.sp@name,start.month,lead)),
                    fname=file.path(anom.dir,gsub("_fragstack.nc$","_anom.nc",basename(fragstack.fname))))

#Define climatology file to use, based on grouping by  lead time and model
in.clim <- subset(anom.meta,forecast.year %in% pcfg@clim.years)
clim.files.l <- split(in.clim,in.clim$clim.fname)

#'==========================================================================
# Calculate climatologies ####
#'==========================================================================
#Loop over climatological files
log_msg("Generating climatologies...\n")
clim.fn <- function(cf) {
  #Setup   
  clim.out.fname <- unique(cf$clim.fname)
  
  #Calculate climatology use CDO - averaging over realisation means (levels) as well
  ensmean.tmp <- tempfile()
  clim.cmd <- cdo("ensmean",
                  cf$fragstack.fname,
                  ensmean.tmp)
  clim.cmd <- cdo("vertmean,weights=F",  #No layer bonds available, so don't weight
                  ensmean.tmp,
                  clim.out.fname)
  unlink((ensmean.tmp))
  return(invisible(NULL))
  
}
dmp <- pblapply(clim.files.l,clim.fn,cl=getOption("mc.cores"))


### 2018.10.05 Functionality removed, remapping is now done in E2
#'==========================================================================
# Setup Remapping weights (in preparation for remapping) ####
#'==========================================================================
#This is a pretty simple process - just subtract everything from everything
#But we also need to take care of the remapping as well, and this is a 
#good place to do it. Remapping weights are therefore setup first
# 
# log_msg("Calculating remapping weights...\n")
# 
# condexec(2,wts.cmd <- cdo(csl("genbil",analysis.grid.fname),
#                             anom.meta$clim.fname[1],
#                             remapping.wts.fname))

#'==========================================================================
# Anomalies, remapping ####
#'==========================================================================
log_msg("Calculating anomalies...\n")

anom.fn <- function(am) {
  #Calculate anomaly
 # anom.temp <- tempfile(fileext = ".nc")
  # anom.cmd <- ncdiff("--netcdf4 --overwrite --history",
  #                    anom.meta$frag.fname[i],
  #                    file.path(lead.clim.dir,anom.meta$clim.fname[i]),
  #                    anom.temp)
  anom.cmd <- cdo("sub",
                  am$fragstack.fname,
                  am$clim.fname,
                  am$fname)

  ### 2018.10.05 Functionality removed, remapping is now done in E2
  # #Remap - using weights
  # # condexec(3,regrid.cmd <- cdo("-f nc",
  # #                              csl("remap", analysis.grid.fname, remapping.wts.fname),
  # #                              anom.temp,
  # #                              am$fname))
  # #Remap - directly
  # condexec(3,regrid.cmd <- cdo("-f nc",
  #                              csl("remapbil", analysis.grid.fname),
  #                              anom.temp,
  #                              am$fname))
  
  return(invisible(NULL))
}
dmp <- pblapply(df2list(anom.meta),anom.fn,cl=getOption("mc.cores"))

#Done. Save the results
saveRDS(anom.meta,file=file.path(base.dir,PE.cfg$files$anom.meta))

#'==========================================================================
# Realization means ####
#'==========================================================================
#Break into chunks per lead time and forecast date
realmean.meta <- mutate(anom.meta,
                        anom.fname=fname,
                        n.realizations=NA,
                        fname=file.path(realmean.dir,
                                        str_replace(basename(anom.fname),
                                                    "anom",
                                                    "realmean")))

#Loop over anomaly files - as these are ultimately based on fragstacks then
#its just a matter of averaging the realisations
log_msg("Processing realization means...\n")
realmean.fn <- function(this.rm) {
  #Average
  realmean.cmd <- cdo("vertmean,weights=F",this.rm$anom.fname,this.rm$fname)

  return(invisible(NULL))
  
}
dmp <- pblapply(df2list(realmean.meta),realmean.fn,cl=getOption("mc.cores"))

#Save data
saveRDS(realmean.meta,file=file.path(base.dir,PE.cfg$files$realmean.meta))


#'==========================================================================
# Complete ####
#'==========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

#'-----------
# This work by Mark R Payne is licensed under a  Creative Commons
# Attribution-NonCommercial-ShareAlike 3.0 Unported License. 
# For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
# Basically, this means that you are free to "share" and "remix" for 
# non-commerical purposes as you see fit, so long as you "attribute" me for my
# contribution. Derivatives can be distributed under the same or 
# similar license.
#
# This work comes with ABSOLUTELY NO WARRANTY or support.
#
# This work should also be considered as BEER-WARE. For details, see
# http://en.wikipedia.org/wiki/Beerware
#'-----------

