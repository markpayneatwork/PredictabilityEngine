#/*##########################################################################*/
#' Extract data from Decadal hindcast archive
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jun  2 15:10:05 2016
#'
#' Extracts hindcast data from DCPP-like outputs and that is stored in a 
#' CDO compatable format
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
cat(sprintf("\n%s\n","Extract Decadal hindcast data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressPackageStartupMessages({
  library(PredEng)
})
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
 if(interactive()) {
  cfg.id <- "MPI.ESM.LR"
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
} else {
  cmd.args <- commandArgs(TRUE)
  if(length(cmd.args)!=1) stop("Cannot get command args")
  cfg.id <- cmd.args
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
}

#Other configurations
set.nco.defaults("--overwrite")

#Retrieve configurations
this.datasrc <- pcfg@Decadal[[cfg.id]]
log_msg("Running with cfg.id %s...\n\n",cfg.id)

#Setup
analysis.grid.fname <- PE.scratch.path(pcfg,"analysis.grid")

#'========================================================================
# Setup ####
#'========================================================================
#Setup database
PE.db.delete.by.datasource(pcfg,PE.cfg$db$extract,this.datasrc)

#Get list of files
src.meta <- tibble(fname=this.datasrc@sources,
                   exists=file.exists(fname))
if(nrow(src.meta)==0 ) stop("No source files provided")
if(any(!src.meta$exists)) stop("Cannot find all source files")

#'========================================================================
# Extract Fragments from Source Files ####
#'========================================================================
#Use the fragment meta data here as the trigging step. My fear is that
#an interruption of the source processing may lead to only a subset of
#the fragments being produced from a given file. Hence, required that
#the source extraction and fragment metadata are run in one large chunk
#all the way to completion.

#Loop over Source Files
log_msg("Extracting fragments from source files...\n")
pb <- PE.progress(nrow(src.meta))
dmp <- pb$tick(0)
for(i in seq(nrow(src.meta))) {
  #Extract file
  f <- src.meta$fname[i]
  log_msg("Extracting from %s...\n",basename(f),silenceable = TRUE)
  tmp.stem <- tempfile()
  
  #Subset out the layer(s) from the field of interest, if relevant
  if(this.datasrc@fields.are.2D ) {
    #Then don't need to do any select
    sym.link <- file.path(getwd(),f)
    if(file.exists(tmp.stem)) file.remove(tmp.stem)
    file.symlink(sym.link,tmp.stem)
    tmp.out <- tmp.stem
  } else { #Field is 3D and requires selection
    tmp.in <- f
    tmp.out <- sprintf("%s_sellevel",tmp.stem)
    if(length(pcfg@vert.range)==1 & all(is.na(pcfg@vert.range))) { 
      #Then we are requesting the surface layer only
      vert.idxs <- 1
    } else {
      #Need to work it out ourselves
      vert.idxs <- get.vertical.levels(pcfg,this.datasrc,tmp.in)
    }
    sellev.cmd <- cdo(csl("sellevidx",vert.idxs),
                      tmp.in,tmp.out)
  }

  #Average over the layers
  tmp.in <- tmp.out
  tmp.out <- sprintf("%s_vertmean",tmp.in)
  levmean.cmd <- cdo("vertmean",tmp.in,tmp.out)
  
  #Select the field of interest, just to be sure
  tmp.in <- tmp.out
  tmp.out <- sprintf("%s_selname",tmp.in)
  selname.cmd <- cdo(csl("selname",this.datasrc@var),tmp.in,tmp.out)
  
  #Before selecting the months of interest, we may need to apply a time
  #correction of the time axis. CESM-DPLE, for example, has the time axis
  #set to 2018-08-01 to represent the period 2018-07-01-2018-08-01, meaning
  #that is actually the average value for July, but is labelled as August. It's a trap!
  #This is where we correct for that effect by copying the time bounds into the
  #time variable, and therefore ensuring that e.g. selmon works properly
  if(!is.na(this.datasrc@use.timebounds)) {
    timebounds.to.time(this.datasrc,tmp.out)
  }
  
  #Select the months of interest 
  tmp.in <- tmp.out
  tmp.out <- sprintf("%s_selmon",tmp.in)
  selmon.cmd <- cdo(csl("selmon", pcfg@MOI),tmp.in,tmp.out)
  
  #Average over time - only necessary when considering multiple target months
  if(pcfg@average.months) {
    tmp.in <- tmp.out
    tmp.out <- sprintf("%s_yearmean",tmp.in)
    yearmean.cmd <- cdo( "yearmean", tmp.in,tmp.out)
  }
  
  #Remap recalculating weights every time
  tmp.in <- tmp.out
  regrid.fname <- sprintf("%s_regrid",tmp.in)
  regrid.cmd <- cdo("-f nc",
                    csl("remapbil", analysis.grid.fname),
                    tmp.in, regrid.fname)
  
  #Import data into raster-land
  dat.b <- readAll(brick(regrid.fname))
  
  #Set CRS status
  #As everything is interpolated onto a common grid, it should also therefore
  #have a CRS reflecting that grid.
  dat.b@crs <- PE.cfg$misc$crs

  #Create metadata
  frag.data <- tibble(srcName=this.datasrc@name,
                      srcType=this.datasrc@type,
                      realization=this.datasrc@realization.fn(f),
                      startDate=this.datasrc@start.date(f),
                      date=this.datasrc@date.fn(regrid.fname),
                      leadIdx=1:nlayers(dat.b),
                      data=as.list(dat.b))
  
  #Write to database
  frag.data %>%
    mutate(startDate=as.character(startDate),
           date=as.character(date)) %>%
    PE.db.appendTable(pcfg,PE.cfg$db$extract)

  #Remove the temporary files to tidy up
  tmp.fnames <- dir(dirname(tmp.stem),pattern=basename(tmp.stem),full.names = TRUE)
  del.err <- unlink(tmp.fnames)
  if(del.err!=0) stop("Error deleting temp files")
  
  #Update progress bar
  pb$tick()
  
}
log_msg("\n")

#Calculate realization means
log_msg("Calculating realization means...\n")
PE.db.calc.realMeans(pcfg,this.datasrc)

#'========================================================================
# Complete 
#'========================================================================
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
#
# Fin
