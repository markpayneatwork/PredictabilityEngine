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
load("objects/PredEng_config.RData")

library(lubridate)
library(raster)
library(tibble)
library(dplyr)

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  src.no <- 1
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent()
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--overwrite")
  do.all <- TRUE #Choose whether to partition out the work, or do it all in a single session
  
} else {
  #Taking inputs from the system environment
  src.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(src.no=="") stop("Cannot find PBS_ARRAYID")
  do.all <- as.logical(Sys.getenv("do.all"))
  if(is.na(do.all)) stop("do.all argument is not set in the system environment")

  #Do everything and tell us all about it
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent(FALSE)
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
}


#Setup configurations
if(pcfg@use.global.ROI) { #only need to use one single global ROI
  this.sps <- list(spatial.subdomain(name="",boundary=pcfg@global.ROI))
} else { #Working with subdomains
  this.sps <- pcfg@spatial.subdomains
  if(!do.all) {
    this.sps <- this.sps[src.no]
  }
}

#Data source
HadISST.dat <- file.path(PE.cfg$datasrc.dir,"Observations/HadISST","HadISST_sst.nc")

#'========================================================================
# Setup Looping ####
#'========================================================================
for(this.sp in this.sps) {
  
  log_msg("\nProcessing %s...\n",this.sp@name)
  
  #Working directories
  subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
  base.dir <- define_dir(subdomain.dir,"Observations","HadISST")
  work.dir <- define_dir(base.dir,"1.Working_files")
  anom.dir <- define_dir(base.dir,"B.anoms")
  clim.dir <- define_dir(base.dir,"A.climatologies")
  analysis.grid.fname <- file.path(subdomain.dir,PE.cfg$analysis.grid.fname)
  

  #/*======================================================================*/
  #'## Process HadISST data
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
  out.fname <- file.path(work.dir,paste0(basename(in.fname),"_remapped"))
  condexec(2,remap.cmd <- cdo("-f nc", csl("remapbil", analysis.grid.fname),
                              in.fname,out.fname))
  
  #monthly extraction
  log_msg("Monthly extraction...")
  in.fname <- out.fname
  out.fname <- paste0(in.fname,"_selmon")
  condexec(1,selmon.cmd <- cdo(csl("selmon",pcfg@MOI),
                               in.fname,out.fname))
  
  #Average over time - only necessary when considering multiple target months
  if(pcfg@average.months) {
    in.fname <- out.fname
    out.fname <- paste0(in.fname,"_yearmean")
    condexec(1,yearmean.cmd <- cdo( "yearmean", in.fname,out.fname))
  }
  
   
  #/*======================================================================*/
  #  Move on to second step of anomalies, clims, fragments
  #/*======================================================================*/
  frag.src <- out.fname
  
  #Calculate climatology 
  log_msg("Climatology....")
  clim.fname <- paste0(frag.src,"_climatology")
  condexec(3,clim.cmd <- cdo("monmean",
                             csl("-selyear",pcfg@clim.years),
                             frag.src,clim.fname))
  
  #Calculate anomalies
  log_msg("Anomalies...")
  anom.fname <- paste0(frag.src,"_anom")
  condexec(4,anom.cmd <- cdo("sub",frag.src,clim.fname,anom.fname))

  #'========================================================================
  # Explode into fragments ####
  #'========================================================================
  #Explode the climatologies fragment into year/months
  log_msg("Exploding...")
  clim.frag.prefix <- file.path(clim.dir,sprintf("%s_climatology_",pcfg@observations@name))
  condexec(3,frag.cmd <- cdo("splitmon",clim.fname,clim.frag.prefix))
  
  #Explode the anomalies fragment into year/months
  log_msg("Exploding...")
  anom.frag.prefix <- file.path(anom.dir,sprintf("%s_",pcfg@observations@name))
  condexec(3,frag.cmd <- cdo("splityearmon",anom.fname,anom.frag.prefix))
  
  #Remove the temporary files to tidy up
  # tmp.fnames <- dir(dirname(temp.stem),pattern=basename(temp.stem),full.names = TRUE)
  # del.err <- unlink(tmp.fnames)
  # if(del.err!=0) stop("Error deleting temp files")
  # 
  
  #/*======================================================================*/
  #  Create (pseudo) metadata for anomalies
  #/*======================================================================*/
  log_msg("Creating pseudo metadata...\n")
  
  #Fragment fnames
  frag.fnames <- dir(anom.dir,pattern=".nc",full.names = TRUE)
  
  #Extract dates
  meta.dat.l <- list()
  for(f in frag.fnames) {
    r <- raster(f)
    meta.dat.l[[f]] <- tibble(date=getZ(r))
  }
  
  #Build metadata
  anom.meta <- bind_rows(meta.dat.l) %>%
    add_column(name=pcfg@observations@name,.before=1) %>%
    add_column(type=pcfg@observations@type,.after=1) %>%
    mutate(start.date=NA,
           n.realizations=1,
           fname=frag.fnames) 
  save(anom.meta,file=file.path(base.dir,"Anomaly_metadata.RData"))
  realmean.meta <- anom.meta
  save(realmean.meta,file=file.path(base.dir,"Realmean_metadata.RData"))
  
  #/*======================================================================*/
  #  And similarly for the climatological files
  #/*======================================================================*/
  log_msg("Creating climatology pseudo metadata...\n")
  
  #Fragment fnames
  clim.frag.fnames <- dir(clim.dir,pattern=".nc",full.names = TRUE)
  
  #Extract dates
  clim.meta.dat.l <- list()
  for(f in clim.frag.fnames) {
    r <- raster(f)
    clim.meta.dat.l[[f]] <- tibble(month=month(getZ(r)))
  }
  
  #Build metadata
  clim.meta <- bind_rows(clim.meta.dat.l) %>%
    add_column(name=pcfg@observations@name,.before=1) %>%
    mutate(type="Climatology",.after=1) %>%
    mutate(fname=clim.frag.fnames) 
  save(clim.meta,file=file.path(base.dir,"Climatology_metadata.RData"))

  # * data.src - name of the datasource
  # * data.type - the type of data. For CMIP5 variables, includes the expt e.g. CMIP5.rcp85
  # * date - of the forecast/observation/projection, not of the model initialisation
  # * start.date - when the forecast is initialisation
  # * n.realizations - the number of realizations stored in the fragment / fragstack
  # * fname - including the full path relative to the project directory
  
  
  
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
