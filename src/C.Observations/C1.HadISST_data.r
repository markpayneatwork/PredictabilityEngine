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
load("objects/setup.RData")

library(lubridate)
library(raster)
library(tibble)
library(dplyr)

#/*======================================================================*/
#  Configuration
#/*======================================================================*/
#Data source
HadISST.dat <- file.path(datasrc.dir,"Observations/HadISST","HadISST_sst.nc")

#Working directories
base.dir <- define_dir(define_dir(file.path(pcfg@scratch.dir,"Observations")),
                       "HadISST")
anom.dir <- define_dir(base.dir,"A.anoms")

set.debug.level(0)  #0 complete fresh run
set.cdo.defaults("--silent --no_warnings")

#/*======================================================================*/
#'## Process HadISST data
#/*======================================================================*/

#If doing a clean run, remove directories etc
if(get.debug.level()<=1) {
  unlink(base.dir,recursive = TRUE,force=TRUE)
  dir.create(base.dir)
  define_dir(anom.dir)
}

#Extract data spatially using CDO and average
#First we need to select the grid, before doing the spatial subsetting,
log_msg("Subsetting data...\n")
in.fname <- HadISST.dat
temp.stem <- tempfile()
out.fname <- temp.stem
condexec(1,annave.cmd <- cdo(csl("sellonlatbox",as.vector(pcfg@ROI)),
                           "-selgrid,lonlat",
                           in.fname,out.fname))

#monthly extraction, and then annual averaging
log_msg("Monthly extraction...\n")
in.fname <- out.fname
out.fname <- paste0(in.fname,"_annave")
condexec(1,annave.cmd <- cdo("yearmean",
                           csl("-selmon",pcfg@MOI),
                           in.fname,out.fname))

#Remap onto the analysis grid
log_msg("Remapping...\n")
in.fname <- out.fname
out.fname <- paste0(in.fname,"_remapped")
condexec(2,remap.cmd <- cdo("-f nc", csl("remapbil", pcfg@analysis.grid),
                           in.fname,out.fname))

#/*======================================================================*/
#  Move on to second step of anomalies, clims, fragments
#/*======================================================================*/
frag.src <- out.fname

#Calculate climatology
log_msg("Climatology....\n")
clim.fname <- file.path(base.dir,"obs_climatology.nc")
condexec(3,clim.cmd <- cdo("timmean",
                         csl("-selyear",pcfg@clim.years),
                         frag.src,clim.fname))

#Calculate anomalies
log_msg("Anomalies...\n")
anom.fname <- file.path(base.dir,"obs_anom.nc")
condexec(4,anom.cmd <- cdo("sub",frag.src,clim.fname,anom.fname))

#Explode the fragment
log_msg("Exploding...\n")
frag.prefix <- file.path(anom.dir,sprintf("%s_",pcfg@observations@name))
condexec(3,frag.cmd <- cdo("splityear",anom.fname,frag.prefix))

#Remove the temporary files to tidy up
tmp.fnames <- dir(dirname(temp.stem),pattern=basename(temp.stem),full.names = TRUE)
del.err <- unlink(tmp.fnames)
if(del.err!=0) stop("Error deleting temp files")


#/*======================================================================*/
#  Create (pseudo) metadata
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

# * data.src - name of the datasource
# * data.type - the type of data. For CMIP5 variables, includes the expt e.g. CMIP5.rcp85
# * date - of the forecast/observation/projection, not of the model initialisation
# * start.date - when the forecast is initialisation
# * n.realizations - the number of realizations stored in the fragment / fragstack
# * fname - including the full path relative to the project directory



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
