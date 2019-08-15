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
#' Extracts HadISST data for subsequent metric analysis. 
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
library(lubridate)
library(raster)
library(tibble)
library(dplyr)

#'========================================================================
# Configuration ####
#'========================================================================
pcfg <- readRDS(PE.cfg$config.path)

#Take input arguments, if any
if(interactive()) {
  cfg.no <- 1
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--overwrite")

} else {
  #Taking inputs from the system environment
  cfg.no <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.no=="") stop("Cannot find LSB_JOBINDEX")

  #Do everything and tell us all about it
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
}

#Retrieve configurations
this.sp <- configure.sp(file.path(PE.cfg$dirs$job.cfg,"Observations.cfg"),cfg.no,pcfg)
this.src <- pcfg@Observations
config.summary(pcfg,this.sp,this.src)

#Data source
HadISST.dat <- unlist(pcfg@Observations@sources)

#'========================================================================
# Setup ####
# If we are considering looping over spatial areas in the one script, this
# is where you would start, by setting this.sp to the appropriate area
# for a list of possibilities
#'========================================================================
log_msg("\nProcessing %s...\n",this.sp@name)

#Working directories
subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
base.dir <- define_dir(subdomain.dir,"Observations","HadISST")
work.dir <- tempdir()
misc.meta.dir <- define_dir(base.dir,PE.cfg$dirs$Misc.meta)
mon.clim.dir <- define_dir(base.dir,"A.monthly_climatologies")
mon.anom.dir <- define_dir(base.dir,"B.monthly_anom")
analysis.grid.fname <- file.path(subdomain.dir,PE.cfg$files$analysis.grid)

#/*======================================================================*/
#'## Extract HadISST data
#/*======================================================================*/
#Extract data spatially using CDO and average
#First we need to select the grid, before doing the spatial subsetting,
#This step is not really necessary, as the analysis grid remapping will
#take care of it much more robustly
# log_msg("Subsetting data...")
# in.fname <- HadISST.dat
# temp.stem <- tempfile()
# out.fname <- temp.stem
# condexec(1,annave.cmd <- cdo(csl("sellonlatbox",as.vector(this.ROI)),
#                              "-selgrid,lonlat",
#                              in.fname,out.fname))

#Remap onto the analysis grid
log_msg("Remapping...")
in.fname <- HadISST.dat
remap.fname <- file.path(work.dir,gsub(".nc$","_remapped.nc",basename(in.fname)))
remap.cmd <- cdo("-f nc", csl("remapbil", analysis.grid.fname),
                  in.fname,remap.fname)

#/*======================================================================*/
#  Anomalies, clims, fragments
#/*======================================================================*/
#Calculate climatology 
log_msg("Climatology....")
mon.clim.fname <- gsub(".nc$","_climatology.nc",remap.fname)
clim.cmd <- cdo("ymonmean",
                           csl("-selyear",pcfg@clim.years),
                           remap.fname,mon.clim.fname)

#Calculate anomalies
log_msg("Anomalies...")
mon.anom.fname <- gsub(".nc$","_anom.nc",remap.fname)
anom.cmd <- cdo("sub",remap.fname,mon.clim.fname,mon.anom.fname)

#'========================================================================
# Average over MOIs (if relevant) ####
#'========================================================================
#Average over time - only necessary when considering multiple target months
if(pcfg@average.months) {
  log_msg("Monthly averaging...")
  
  #Setup MOI directories
  MOIave.clim.dir <- define_dir(base.dir,"C.MOIave_climatology")
  MOIave.anom.dir <- define_dir(base.dir,"D.MOIave_anoms")
  
  #Create a function to do this (as we need to reuse the code for
  #both the climatology and the anomalies)
  MOI.average <- function(in.src) {
    #monthly extraction
    out.fname <- gsub(".nc$","_selmon.nc",in.src)
    selmon.cmd <- cdo(csl("selmon",pcfg@MOI),
                                 in.src,out.fname)
    
    #Calculate means of the anomalies
    in.fname <- out.fname
    out.fname <- gsub(".nc$","_yearmean",in.fname)
    yearmean.cmd <- cdo( "yearmean", in.fname,out.fname)
    
    return(out.fname)
  }
  
  #Now do averaging
  MOIave.anom <- MOI.average(remap.fname)
  MOIave.yearmean <- MOI.average(mon.clim.fname)
  
  #Now need to do the complete mean on MOIave.clim and move it 
  #to the appropriate directory
  MOIave.clim <- file.path(MOIave.clim.dir,"MOIave_climatology.nc")
  clim.cmd <- ncwa("-a time", 
                              MOIave.yearmean,
                              MOIave.clim)
  
}

#'========================================================================
# Explode into fragments ####
#'========================================================================
log_msg("Fragmenting...")

#Explode the climatologies fragment into year/months
mon.clim.frag.prefix <- file.path(mon.clim.dir,sprintf("%s_climatology_",this.src@name))
frag.cmd <- cdo("splitmon",mon.clim.fname,mon.clim.frag.prefix)

#Explode the anomalies fragment into year/months
mon.anom.frag.prefix <- file.path(mon.anom.dir,sprintf("%s_",this.src@name))
frag.cmd <- cdo("splityearmon",mon.anom.fname,mon.anom.frag.prefix)

if(pcfg@average.months) {
  #Explode the MOIanomalies fragment into individual files, one per year
  MOIave.anom.frag.prefix <- file.path(MOIave.anom.dir,sprintf("%s_",this.src@name))
  MOI.frag.cmd <- cdo("splityear",
                                 MOIave.anom,
                                 MOIave.anom.frag.prefix)
  
}

#/*======================================================================*/
#  Create (pseudo) metadata 
#/*======================================================================*/
log_msg("Creating pseudo metadata...\n")

#Use a generic function to do the hardwork
generate.metadata <- function(src.dir) {
  #Get fnames
  src.fnames <- dir(src.dir,pattern=".nc",full.names = TRUE)
  
  #Extract dates
  meta.dat.l <- list()
  for(f in src.fnames) {
    r <- raster(f)
    meta.dat.l[[f]] <- tibble(date=getZ(r))
  }
  
  #Build metadata
  src.meta <- bind_rows(meta.dat.l) %>%
    add_column(src.name=this.src@name,
               src.type=this.src@type,
               .before=1) %>%
    mutate(start.date=NA,
#           n.realizations=1,
           fname=src.fnames) 
  return(src.meta)
}

#Now, lets think for a minute. The downstream functions require two 
#files - Anomaly_metadata.RData and Realmean_metadata.RData. The choice
#of whether these relate to individual months or to an MOIaverage should
#be made here, not downstream, so we therefore need to set these up according
#to the project configuration. At the same time, we also want to store all
#metadata, so that it can be picked up later by the persistence forcast code
#So....
#First generate all monthly metadata anomalies - we need this for the persistence
#forecast anyway
mon.anom.meta <- generate.metadata(mon.anom.dir)
saveRDS(mon.anom.meta,file=file.path(base.dir,PE.cfg$files$Obs.monthly.anom.metadata))

#Now, setup rest of metadata accordingly
if(pcfg@average.months) {
  #Get metadata
  anom.meta <- generate.metadata(MOIave.anom.dir)
} else {
  #We are only interested in files that are in the
  #months of interest, so we need to filter
  anom.meta <- subset(mon.anom.meta,month(date) %in% pcfg@MOI)
}

#Save results and create a second copy as realmean metadata
saveRDS(anom.meta,file=file.path(base.dir,PE.cfg$files$anom.meta))
realmean.meta <- anom.meta  #Needs a rename
saveRDS(realmean.meta,file=file.path(base.dir,PE.cfg$files$realmean.meta))

#And now for the climatologies
if(pcfg@average.months) {
  #Only a single clim file - generate by hand
  clim.meta <- tibble(src.name=this.src@name,
                      src.type="Climatology",
                      date=as.Date(ISOdate(9999,pcfg@MOI,15)),
                      start.date=NA,
#                      n.realizations=1,
                      fname=MOIave.clim)
  
} else {
  #Generate a climatology 
  clim.meta <- generate.metadata(mon.clim.dir)
  clim.meta$src.type <- "Climatology"
  
  #Restrict to months in the MOI
  clim.meta <- subset(clim.meta,month(date) %in% pcfg@MOI)
}
saveRDS(clim.meta,file=file.path(base.dir,PE.cfg$files$Obs.climatology.metadata))

#Remove the temporary files to tidy up
tmp.fnames <- dir(work.dir,pattern=work.dir,full.names = TRUE)
del.err <- unlink(tmp.fnames)
if(del.err!=0) stop("Error deleting temp files")

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
