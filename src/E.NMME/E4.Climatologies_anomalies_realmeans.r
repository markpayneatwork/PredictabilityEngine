#'##########################################################################
# E4.Climatologies, Anomalies and Realization Means
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
cat(sprintf("\n%s\n","E4.Climatologies, Anomalies and Realization Means"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
load("objects/configuration.RData")
load("objects/PredEng_config.RData")

#'==========================================================================
# Configure ####
#'==========================================================================
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
}

#Other configurations
set.nco.defaults("--overwrite")

#Retrieve configurations
this.sp <- get.this.sp(file.path(PE.cfg$dirs$cfg,"NMME.cfg"),cfg.no,pcfg)
this.src <- get.this.src(file.path(PE.cfg$dirs$cfg,"NMME.cfg"),cfg.no,pcfg)

#Configure directories
subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
base.dir <- define_dir(subdomain.dir,"NMME",this.src@name)
fragstack.dir <- define_dir(base.dir,"1.fragstacks")
lead.clim.dir <- define_dir(base.dir,"2.lead.clims")
anom.dir <- define_dir(base.dir,"A.anoms")
realmean.dir <- define_dir(base.dir,"B.realmean")
analysis.grid.fname <- file.path(subdomain.dir,PE.cfg$files$analysis.grid)
remapping.wts.fname <- file.path(base.dir,PE.cfg$files$remapping.wts)

log_msg("Configuration.\n")
log_msg("--------------\n")
log_msg("Data source       : %s \n",this.src@name)
log_msg("Spatial subdomain : %s (%s)\n",this.sp@name,this.sp@desc)


#'==========================================================================
# Setup ####
#'==========================================================================
#Load fragstack metadata
load(file.path(base.dir,PE.cfg$files$fragstack.meta))

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
pb <- progress_estimated(length(clim.files.l))
for(cf in clim.files.l) {
  #Update progress bar
  pb$tick()$print()
  
  #Setup   
  clim.out.fname <- unique(cf$clim.fname)

  #Calculate climatology use CDO - averaging over realisation means (levels) as well
  ensmean.tmp <- tempfile()
  condexec(1,clim.cmd <- cdo("ensmean",
                             cf$fragstack.fname,
                             ensmean.tmp))
  condexec(1,clim.cmd <- cdo("vertmean",
                             ensmean.tmp,
                             clim.out.fname))
  unlink((ensmean.tmp))
  
}
Sys.sleep(0.1)
print(pb$stop())

#'==========================================================================
# Setup Remapping weights (in preparation for remapping) ####
#'==========================================================================
#This is a pretty simple process - just subtract everything from everything
#But we also need to take care of the remapping as well, and this is a 
#good place to do it. Remapping weights are therefore setup first

log_msg("Calculating remapping weights...\n")

condexec(2,wts.cmd <- cdo(csl("genbil",analysis.grid.fname),
                            anom.meta$clim.fname[1],
                            remapping.wts.fname))

#'==========================================================================
# Anomalies, remapping ####
#'==========================================================================
log_msg("Calculating anomalies...\n")
pb <- progress_estimated(nrow(anom.meta))
for(i in seq(nrow(anom.meta))) {
  #Update progress bar
  pb$tick()$print()
  am <- anom.meta[i,]
  
  #Calculate anomaly
  anom.temp <- tempfile(fileext = ".nc")
  # anom.cmd <- ncdiff("--netcdf4 --overwrite --history",
  #                    anom.meta$frag.fname[i],
  #                    file.path(lead.clim.dir,anom.meta$clim.fname[i]),
  #                    anom.temp)
  anom.cmd <- cdo("sub",
                  am$fragstack.fname,
                  am$clim.fname,
                  anom.temp)
  
  condexec(3,anom.cmd)
  
  #Remap
  condexec(3,regrid.cmd <- cdo("-f nc",
                               csl("remap", analysis.grid.fname, remapping.wts.fname),
                               anom.temp,
                               am$fname))
}

Sys.sleep(0.1)
print(pb$stop())

#Done. Save the results
save(anom.meta,file=file.path(base.dir,"Anom_metadata.RData"))

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
pb <- progress_estimated(nrow(realmean.meta))
for(i in seq(nrow(realmean.meta))) {
  #Update counter
  pb$tick()$print()
  this.rm <- realmean.meta[i,]
  
  #Average
  realmean.cmd <- cdo("vertmean",this.rm$anom.fname,this.rm$fname)
  condexec(4,realmean.cmd)
  
}
Sys.sleep(0.1)
print(pb$stop())

#Save data
save(realmean.meta,file=file.path(base.dir,"Realmean_metadata.RData"))


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

