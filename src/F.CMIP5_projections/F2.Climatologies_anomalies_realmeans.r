#'========================================================================
# F2.Retrieve_metadata.r
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed May 23 10:50:49 2018
#
# Extracts metadata from the fragments created in script F1, for use in subsequent
# processing
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","F2.Retrieve_metadata.r"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(PredEng)
library(ncdf4)
library(tidyverse)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.id <- 1
  set.cdo.defaults("--silent -O --no_warnings")
  set.log_msg.silent(TRUE)
} else {
  #Taking inputs from the system environment
  cfg.id <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.id=="") stop("Cannot find LSB_JOBINDEX")
  #Do everything
  set.log_msg.silent(FALSE)
}
set.nco.defaults("--netcdf4 --overwrite --history")

#Retrieve configurations
cfg.fname <- file.path(PE.cfg$dirs$job.cfg,"CMIP5_by_sources.cfg")
this.cfgs <- get.cfgs(cfg.fname)
this.sp <- configure.sp(cfg.fname,cfg.id,pcfg)
this.src <- configure.src(cfg.fname,cfg.id,pcfg)
config.summary(pcfg,this.sp,this.src)

#Directory setup
CMIP5.dir <- define_dir(pcfg@scratch.dir,"CMIP5")
base.dir <- define_dir(CMIP5.dir,this.src@name)
frag.dir <- define_dir(base.dir,"3.fragments")
fragstack.dir <- define_dir(base.dir,"4.fragstacks")
clim.dir <- define_dir(base.dir,"5.clim")
anom.dir <- define_dir(base.dir,"A.anom")
realmean.dir <- define_dir(base.dir,"B.realmean")
ensmean.dir <- define_dir(base.dir,"C.ensmean")
misc.meta.dir <- define_dir(base.dir,PE.cfg$dirs$Misc.meta)

#'========================================================================
# Setup ####
#'========================================================================
fragstack.meta <- readRDS(file.path(base.dir,PE.cfg$files$fragstack.meta))

#'========================================================================
# Calculate climatologies ####
# The climatology is a bit messy to calculate in this context, because of
# the fact that we have the historical experiments that end in 2005 and 
# then we split into multiple rcp experiments. We simplify this problem
# by focusing only on the historical experiment for the purpose of defining
# the climatology - we also make this redefinition through the rest of
# the configuration. This may cause some problems in the future, although
# it is hard to see at the moment where, because we have a comparison
# period as well, which is used for estimating skill metrics. Anyway,
# we simplify things by not allowing climatologies to extend beyond 2005
# and then proceed accordingly
#'========================================================================
log_msg("Calculating climatologies...\n")
if(any(pcfg@clim.years>2005)) stop("Climatology years extend beyond 2005.")

#Setup metadata table for next round of processing (climatologies and anoms)
anom.meta <-  mutate(fragstack.meta,
                     src.type=sprintf("%s.%s","CMIP5",src.expt),
                     start.date=NA,
                     fragstack.fname=fname,
                     fname=file.path(anom.dir,sprintf("%s_%s_%s_anom.nc",src.name,src.expt,year(date))),
                     clim.fname=file.path(clim.dir,sprintf("%s_clim.nc",src.name)))

#Now select the files to work with. As we have fragstacks, we can select for
#both year and experiment simultaneously
clim.meta <- subset(anom.meta,src.expt=="historical" & year(date) %in% pcfg@clim.years)

# Calculate climatologies
# Calculating the climatology could be complicated by the fact
# that not all of the fragstacks that would go into the climatology 
# necessarily have the same number of realizations. If not, then this
# requires averaging over the fragstack first and then over the years.
# We check for this along the way
# Prescreening of CMIP5 files should avoid this, but it is still worth checking

#Check that all fragstacks have the same number of realizations. If not, 
#throw an error (to start with)
unique.n.reals <- table(clim.meta$n.realizations)
if(length(unique.n.reals)!=1) {
  stop("Uneven number of realizations in selected fragstacks")
}

#Calculate the climatology
#We need to use nco here, rather than CDO - not sure why, but it
#seems that the dimensionality is a bit too strange for CDO
#First we do the ensemble averaging, then we need to average
#over the realization climatologies to get the total climatology
#Note that we need to do the averaging with ncwa rather than ncra so 
#anomaly creation via ncdiff works properly
realclim.tmp <- tempfile()
realclim.cmd <- ncra(clim.meta$fragstack.fname,realclim.tmp)
clim.cmd2 <- ncwa("-a tos,realization", 
                  realclim.tmp,unique(clim.meta$clim.fname))

#'========================================================================
# Calculate anomalies ####
#'========================================================================
log_msg("Anomalies...\n")

#Calculate the anomalies per realisation
pb <- progress_estimated(nrow(anom.meta))
for(m in seq(nrow(anom.meta))){
  pb$tick()$print()
  am <- anom.meta[m,]
  log_msg("Calculating anomalies for %s...\n",
          basename(am$fname),silenceable = TRUE)

  #Calculate the anomaly
  anom.cmd <- ncdiff(am$fragstack.fname,am$clim.fname,am$fname)
}
Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

saveRDS(anom.meta,file=file.path(base.dir,PE.cfg$files$anom.meta))

#'========================================================================
# Calculate realisation means ####
# This should be simple, because we have everything in place in the form
# of fragstack anomalies - we just average over them
#'========================================================================
log_msg("Realisation means...\n")

#Break into chunks per model, experiment
realmean.meta <- anom.meta %>%
                 mutate(anom.fname=fname,
                        fname=file.path(realmean.dir,
                                        gsub("anom.nc",
                                             "realmean-anom.nc",
                                             basename(fname))),
                        n.realizations=1,
                        fragstack.fname=NULL)

#Average over the fragstack anomalies at given lead time
pb <- progress_estimated(nrow(realmean.meta))
for(i in seq(nrow(realmean.meta))) {
  pb$tick()$print()
  rlm <- realmean.meta[i,]
  log_msg("Calculating realmean %i of %i...\n",
          i,nrow(realmean.meta),
          silenceable = TRUE)

  #Perform averaging over realizations
  realmean.cmd <- ncwa("-a realization",
                                  rlm$anom.fname,rlm$fname)
  
}

Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

#Save realmeta data
saveRDS(realmean.meta,file=file.path(base.dir,PE.cfg$files$realmean.meta))

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("Analysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# .............
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
# .............
