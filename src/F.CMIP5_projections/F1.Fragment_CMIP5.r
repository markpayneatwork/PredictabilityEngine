#*##########################################################################*/
#' Extract data from CMIP5 projections
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Tue Sep 13 12:03:25 2016
#'
#' Extracts data from CMIP5 projections
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
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Extract CMIP5 data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ncdf4)
library(tidyverse)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.id <- 3
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
} else {
  #Taking inputs from the system environment
  cfg.id <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.id=="") stop("Cannot find LSB_JOBINDEX")
  #Do everything
  set.log_msg.silent(silent=FALSE)
}

#Retrieve configurations
cfg.fname <- file.path(PE.cfg$dirs$job.cfg,"CMIP5_by_sources.cfg")
this.cfgs <- get.this.cfgs(cfg.fname)
this.sp <- get.this.sp(cfg.fname,cfg.id,pcfg)
this.src <- get.this.src(cfg.fname,cfg.id,pcfg)
config.summary(pcfg,this.sp,this.src)

#Directory setup
CMIP5.dir <- define_dir(pcfg@scratch.dir,"CMIP5")
base.dir <- define_dir(CMIP5.dir,this.src@name)
wts.dir <- define_dir(base.dir,"1.remapping_wts")
remap.dir <- define_dir(base.dir,"2.remap")
frag.dir <- define_dir(base.dir,"3.fragments")
analysis.grid.fname <- file.path(pcfg@scratch.dir,PE.cfg$files$analysis.grid)

#Error checking
if(length(pcfg@MOI)!=1) stop("CMIP5 processing currently only supports single month extraction")

#'========================================================================
# Extract data ####
#'========================================================================
#Get list of fnames
src.fnames <- unlist(this.src@sources)

# #Prepare a set of remapping weights
# HERE THERE BE DRAGONS: Because the models are inherently on different grids
# we should really use a different set of weights for each model. It's probably
# safer just to avoid this approach
# log_msg("Preparing weights...\n")
# for(mdl in names(mdl.l)) {
#   remapping.wts <- file.path(wts.dir,sprintf("%s_remapping_wts.nc",mdl))
#   mdl.file.example <- subset(CMIP5.meta,model==mdl)$fname[1]
#   wts.cmd <- cdo(csl("genbil",analysis.grid.fname),
#                           mdl.file.example,remapping.wts)
# }

#Loop over files and extract the key data from each file
log_msg("Extracting data...\n",silent=FALSE)
pb <- progress_estimated(length(src.fnames))
pb$print()
for(i in seq(length(src.fnames))) {

  #Extract file
  this.f <- src.fnames[i]
  temp.stem <- tempfile()
  log_msg("Fragmenting %s...\n",basename(f),silenceable = TRUE)
 
  #Subset out the surface layer from the field of interest
  #TODO: Note that we will probably need to change this in the future
  #to allow sub-surface properties - however, this is a bit of work
  #so we leave it for the moment.
  temp.in <- this.f
  temp.out <- sprintf("%s_sellevidx",temp.stem)
  sellev.cmd <- cdo("sellevidx,1",temp.in,temp.out)
  
  #Select the field of interest, just to be sure
  temp.in <- temp.out
  temp.out <- sprintf("%s_selname",temp.in)
  selname.cmd <- cdo(csl("selname",this.src@var),temp.in,temp.out)

  #Select the months of interest 
  temp.in <- temp.out
  temp.out <- sprintf("%s_selmon",temp.in)
  selmon.cmd <- cdo(csl("selmon", pcfg@MOI),temp.in,temp.out)

  #Remap
  temp.in <- temp.out
  remap.fname <- file.path(remap.dir,basename(this.f))
  remap.cmd <- cdo("-f nc", 
                   csl("remapbil", analysis.grid.fname),
                   temp.in, remap.fname)
  
  #Remove the temporary files to tidy up
  tmp.fnames <- dir(dirname(temp.stem),pattern=basename(temp.stem),full.names = TRUE)
  del.err <- unlink(tmp.fnames)
  if(del.err!=0) stop("Error deleting temp files")
  
  #Now finally split into fragments 
  frag.prefix <- file.path(frag.dir,
                sprintf("%s_%s_%s_",CMIP5_model(this.f),CMIP5_experiment(this.f),CMIP5_realisation(this.f)))
  frag.cmd <- cdo("splityear",remap.fname,frag.prefix)

  #Tick
  pb$tick()$print()
}

print(pb$stop())
log_msg("\n")

#'========================================================================
# Complete ####
#'========================================================================
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
#
# Fin
