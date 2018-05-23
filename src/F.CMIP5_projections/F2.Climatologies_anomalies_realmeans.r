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

library(tidyverse)
library(PredEng)
library(dplyr)
library(lubridate)
load("objects/configuration.RData")

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  # mdl.no <- 5
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent()
} else {
  #Taking inputs from the system environment
  #  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  #if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}

#Directory setup
src.dir <- file.path("data_srcs","CMIP5")
base.dir <- define_dir(pcfg@scratch.dir,"CMIP5")
frag.dir <- define_dir(base.dir,"3.fragments")
clim.dir <- define_dir(base.dir,"4.clim")
anom.dir <- define_dir(base.dir,"A.anom")
realmean.dir <- define_dir(base.dir,"B.realmean")
ensmean.dir <- define_dir(base.dir,"C.ensmean")

#Check that climatology range is valid
if(any(pcfg@clim.years>2005))stop("Climatology years extend beyond 2005.")

#'========================================================================
# Setup ####
#'========================================================================
#Get list of fragments
frag.fnames <- dir(frag.dir,pattern=".nc",full.names = TRUE,recursive=TRUE)
if(length(frag.fnames)==0) stop("Cannot find fragment files")

#Fragment metadata

#'========================================================================
# Retrieve metadata ####
#'========================================================================
log_msg("Loading date metadata...\n")

#Loop over the individual files getting the date information
date.meta.l  <- list()
pb <- progress_estimated(length(frag.fnames))
for(f in frag.fnames) {
  pb$tick()$print()
  #Doing it with a NetCDF read is faster, but doesn't
  #do a very good job of parsing the dates correctly
  #We use raster instead
  b <- brick(f)
  b.dates <- getZ(b)
  date.meta.l[[f]] <-  tibble(start.date= min(b.dates),
                              n.dates=length(b.dates),
                              end.date=max(b.dates))
}
Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

#Form metadata table
frag.meta <- tibble(model=underscore_field(frag.fnames,1),
                     expt=underscore_field(frag.fnames,2),
                     realization=underscore_field(frag.fnames,3)) %>%
                  bind_cols(bind_rows(date.meta.l)) %>%
                  add_column(fname=frag.fnames)

save(frag.meta,file=file.path(base.dir,"Fragment_metadata.RData"))

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

#Setup metadata table for next round of processing (climatologies and anoms)
anom.meta <-  mutate(frag.meta,
                     anom.fname=file.path(anom.dir,sprintf("%s_%s_%s_anom.nc",model,expt,realization)),
                     clim.fname=file.path(clim.dir,sprintf("%s_clim.nc",model)))

#Now select the files to work with
hist.meta <- subset(anom.meta,expt=="historical")
clim.grp.l <- split(hist.meta,hist.meta[,c("model")])

#Loop over historical files to calculate climatologies
#We take a lazy strategy here and first average across
#all realizations, then select the years that we want
#and then finally calculate the climatological field
#In cases where there are differences in the length of
#the historical runs, we will have to do something different
#but have added an error check for this.
pb <- progress_estimated(length(clim.grp.l))
for(g in clim.grp.l) {
  pb$tick()$print()
  
  #Check that all realizations have the same length
  if(length(unique(g$n.dates))!=1) stop("Not all realizations have the same length")
  
  #Calculate the realization mean first
  real.mean.fname <- tempfile()
  condexec(5,realmean.cmd <- cdo("-O ensmean",g$fname,real.mean.fname))
  
  #Calculate the climatology across the years of interest 
  condexec(5,clim.cmd <- cdo("timmean",
                           csl("-selyear",pcfg@clim.years),
                           real.mean.fname,
                           unique(g$clim.fname)))
  
  #Remove the temp file
  del.err <- unlink(real.mean.fname)
  if(del.err!=0) stop("Error deleting temp files")
  
}

Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

#'========================================================================
# Calculate anomalies ####
#'========================================================================
log_msg("Anomalies...\n")

#Calculate the anomalies per realisation
pb <- progress_estimated(nrow(anom.meta))
for(m in seq(nrow(anom.meta))){
  pb$tick()$print()
  condexec(6,anom.cmd <- cdo("sub",
                           anom.meta$fname[m],
                           anom.meta$clim.fname[m],
                           file.path(anom.dir,basename(anom.meta$anom.fname)[m])))
}
Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

save(anom.meta,file=file.path(base.dir,"Anom_metadata.RData"))

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
