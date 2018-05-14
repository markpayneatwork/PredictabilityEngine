#/*##########################################################################*/
#' Extract HadISST data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jul 14 11:34:17 2016
#'
#' Extracts HadISST data for subsequent metric analysis
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
cat(sprintf("\n%s\n","Extract HadISST data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")

library(lubridate)
library(raster)

#/*======================================================================*/
#  Configuration
#/*======================================================================*/
#Data source
HadISST.dat <- "data_srcs/HadISST_sst.nc"

#Working directories
base.dir <- file.path("processing",pcfg@name)
dat.dir <- file.path(base.dir,"HadISST")

options("run.level"= 1)  #1 complete fresh run

#/*======================================================================*/
#'## Process HadISST data
#/*======================================================================*/
log_msg("Subsetting data...\n")

#If doing a clean run, remove directories etc
if(options("run.level")$run.level<=1) {
  unlink(dat.dir,recursive = TRUE,force=TRUE)
  dir.create(dat.dir)
}

#Extract data spatially using CDO and average
#First we need to select the grid, before doing the spatial subsetting,
ROI.fname<- file.path(dat.dir,"observations_ROI.nc")
run_if(1,annave.cmd <- cdo(csl("sellonlatbox",as.vector(pcfg@ROI)),
                           "-selgrid,lonlat",
                           HadISST.dat,ROI.fname))

#monthly extraction, and annual averaging
annave.fname<- file.path(dat.dir,"observations_annave.nc")
run_if(1,annave.cmd <- cdo("yearmean",
                           csl("-selmonth",pcfg@MOI),
                           ROI.fname,annave.fname))

#Remap onto the analysis grid
log_msg("Remapping...\n")
remap.fname <- file.path(dat.dir,"observations.nc")
run_if(2,remap.cmd <- cdo("-f nc", csl("remapbil", pcfg@analysis.grid),
                           annave.fname, remap.fname))


#Calculate climatology
log_msg("Climatology....\n")
clim.fname <- file.path(dat.dir,"obs_climatology.nc")
run_if(3,clim.cmd <- cdo("timmean",
                         csl("-selyear",pcfg@clim.years),
                         remap.fname,clim.fname))

#Calculate anomalies
log_msg("Anomalies...\n")
anom.fname <- file.path(dat.dir,"obs_anom.nc")
run_if(4,anom.cmd <- cdo("sub",remap.fname,clim.fname,anom.fname))

# #Cross check that this worked corretly using Raster
# b.anom <- brick(anom.fname)
# b.clim <- brick(clim.fname)
# b.obs <- brick(annave.fname)
# b.obs.rec <- b.anom+b.clim  #Reconstructed
# b.diff <- b.obs - b.obs.rec
# #Check anomaly summation over the cliamtological period
# clim.yr.idxs <- which(year(getZ(b.anom)) %in% pcfg@clim.years)
# b.anom.sum <- sum(b.anom[[clim.yr.idxs]])

#/*======================================================================*/
#  Make some analyses to check the validity
#/*======================================================================*/
# #Annual data coverage
# b <- brick(ROI.fname)
# missing <- cellStats(is.na(b),stat = "mean")
# 
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
