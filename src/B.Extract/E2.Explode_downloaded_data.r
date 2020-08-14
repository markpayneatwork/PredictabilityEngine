#/*##########################################################################*/
#' E2. Explode downloaded data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Tue May 15 13:29:02 2018
#'
#' NMME data is inherently 5D when it is downloaded (lat, lon, start date, lead,
#' realization). To get it into a format where it can be picked up by the rest
#' of the codebase, we need to "explode" it into fragstacks so that the pieces 
#' are 3D in nature (lon, lat, realization), one for each start date and lead.
#' We do this based primarily on the ncks tool. Ideally, we would also like to
#' end with files that are compatible with CDO, but we shall see if that is possible
#' 
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
cat(sprintf("\n%s\n","E2. Explode Downloaded Data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tidyverse)
library(reshape2)
library(ncdf4)
library(raster)
library(parallel)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.no <- 5
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--ovewrite")
  options("mc.cores"=1)  
  
} else {
  #Taking inputs from the system environment
  cfg.no <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.no=="") stop("Cannot find LSB_JOBINDEX")
  #Do everything and tell us all about it
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
  options("mc.cores"= as.numeric(Sys.getenv("LSB_MAX_NUM_PROCESSORS")))
}

#Other configurations
set.nco.defaults("--overwrite")

#Retrieve configurations
cfg.file <- file.path(PE.cfg$dirs$job.cfg,"NMME_by_sources.cfg")
cfgs <- get.cfgs(cfg.file)
this.sp <- global.ROI(pcfg)
this.src <- configure.src(cfg.file,cfg.no,pcfg)

#Configure directories
base.dir <- define_dir(pcfg@scratch.dir,
                       "NMME",this.src@name)
data.dir <- file.path(PE.cfg$dirs$datasrc,"NMME",this.src@name)

fragstack.dir <- define_dir(base.dir,"1.fragstacks")
misc.meta.dir <- define_dir(base.dir,PE.cfg$dirs$Misc.meta)

analysis.grid.fname <- file.path(pcfg@scratch.dir,PE.cfg$files$analysis.grid)

#Display configuration
config.summary(pcfg,cfg.no,this.src,this.sp)

#'========================================================================
# Setup ####
#'========================================================================
#Get metadata of files available
downloaded.fnames <- dir(data.dir,full.names = TRUE,pattern=".nc$")

#'========================================================================
# Explode data ####
#'========================================================================
#Loop over files
fragstack.meta.l <- list()
pb <- progress_estimated(length(downloaded.fnames))

log_msg("Exploding files...\n")
for(i in seq(downloaded.fnames)) {
  this.file <- downloaded.fnames[i]
  
  #log_msg("Exploding %s, %s, file %02i of %02i...\n",this.sp@name,basename(this.file),i,length(downloaded.fnames))
  
  #Figure out what's available, and what we actually want to extract
  ncid <- nc_open(this.file)
  all.SL <- expand.grid(S.idx=seq(ncid$dim$S$len),
                        L.idx=seq(ncid$dim$L$len)) %>%
    as_tibble() %>%
    mutate(S.val=ncid$dim$S$val[S.idx],
           L.val=ncid$dim$L$val[L.idx],
           start.date=PE.cfg$misc$NMME.epoch.start+months(S.val),
           forecast.date=start.date+months(floor(L.val)),
           forecast.month=month(forecast.date))
  nc_close(ncid)
  
  #Now restrict to the relevant months
  sel.SL <- subset(all.SL,forecast.month %in% pcfg@MOI)
  
  #Now comes the mega loop, where we extract the dimensions that
  #we are looking for
  frag.fn <- function(this.sel.SL){
    #Setup for explode
    fragstack.fname <- sprintf("%s_%s_S%s_L%02.1f_fragstack.nc",
                              this.src@name,
                              this.sp@name,
                              format(this.sel.SL$start.date,"%Y%m%d"),
                              this.sel.SL$L.val)
    fragstack.full.path <- file.path(fragstack.dir,fragstack.fname)
    frag.temp <- tempfile(fileext=".nc")
    
    SL.ROI.str <- sprintf("-d S,%i -d L,%i",
                           this.sel.SL$S.idx,
                           this.sel.SL$L.idx)
    
    #Boomski! Do it.
    explode.cmd <- ncks("--fortran --deflate 0",   #Use indexing starting at 1, like in R
                        SL.ROI.str,
                        this.file,
                        frag.temp)

    #This leaves us with a file that is still 5D, but there are two
    #degenerate dimensions now. If we drop them, then we end up with
    #something that CDO can work with. For safety's sake, we first
    #copy the two values into attributes
    # ncid <- nc_open(frag.temp)
    # nc_close(ncid)
    # condexec(1,cmd <-ncatted("-h", sprintf("-a start.date,global,c,f,%f",ncid$dim$S$val),
    #                          frag.temp,frag.temp))
    # condexec(1,cmd <-ncatted("-h", sprintf("-a lead,global,c,f,%f",ncid$dim$L$val),
    #                          fragstack.full.path,fragstack.full.path))
    
    #Now drop the start.date and lead dimensions
    ncwa.cmd <- ncwa(sprintf("-a %s,S,L",this.src@var),
                     frag.temp,frag.temp)

    
    #This leaves us with something that is compatible with CDO. Now we can do the
    #remapping onto the grid of interest.
    regrid.cmd <- cdo("-f nc4",
                      csl("remapbil", analysis.grid.fname),
                      frag.temp,
                      fragstack.full.path)
    
    #Finally, drop the temp file
    file.remove(frag.temp)
    
    #Create metadata as we go
    res <- mutate(this.sel.SL,
                  fname=fragstack.full.path,
                  n.realizations=ncid$dim$M$len)
    return(res)}
  
  #Parallelise it - but only if there is something to parallelise!
  if(nrow(sel.SL)!=0) {
    rtn.l <- mclapply(df2list(sel.SL),frag.fn)
    fragstack.meta.l[[this.file]] <- bind_rows(rtn.l)
  }
  pb$tick()$print()
}
pb$stop()
log_msg("\n")

#'========================================================================
# Finish 
#'========================================================================
#Build meta data object
fragstack.meta <- bind_rows(fragstack.meta.l) %>%
  select(start.date,date=forecast.date,lead=L.val,n.realizations,fname) %>%
  add_column(src.name=this.src@name,
             src.type=this.src@type,
             .before=1)

saveRDS(fragstack.meta,file=file.path(base.dir,PE.cfg$files$fragstack.meta))

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
