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

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ncdf4)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.id <- 2
  set.cdo.defaults("--silent --no_warnings -O")
  #set.cdo.defaults("-O")
  set.log_msg.silent()
  set.nco.defaults("--ovewrite")
  pcfg@recalculate <- TRUE
} else {
  #Taking inputs from the system environment
  cfg.id <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.id=="") stop("Cannot find LSB_JOBINDEX")
  #Do everything and tell us all about it
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
}

#Other configurations
set.nco.defaults("--overwrite")

#Retrieve configurations
cfg.fname <- file.path(PE.cfg$dirs$job.cfg,"Decadal_by_chunks.cfg")
this.cfgs <- get.this.cfgs(cfg.fname)
this.sp <- get.this.sp(cfg.fname,cfg.id,pcfg)
this.chunk <- get.this.src(cfg.fname,cfg.id,pcfg)
config.summary(pcfg,this.sp,this.chunk)

#Directory setup
subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
datsrc.dir <- define_dir(subdomain.dir,"Decadal",this.chunk@name)
chunk.dir <- define_dir(datsrc.dir,this.chunk@chunk)
remap.dir <- define_dir(chunk.dir,"1.remapping_wts")
sel.dir <- define_dir(chunk.dir,"2.regrid")
frag.dir <- define_dir(chunk.dir,"3.fragments")
fragstack.dir <- define_dir(chunk.dir,"4.fragstacks")
misc.meta.dir <- define_dir(chunk.dir,PE.cfg$dirs$Misc.meta)
analysis.grid.fname <- file.path(subdomain.dir,PE.cfg$files$analysis.grid)

#'========================================================================
# Setup ####
#'========================================================================
log_msg("Processing %s data source for %s subdomain ...\n",this.chunk@name,this.sp@name)

#Get list of files
src.fnames <- unlist(this.chunk@sources)
if(length(src.fnames)==0 ) stop("Cannot find source files")
if(any(!file.exists(src.fnames))) stop("Cannot find all source files")
src.meta <- tibble(fname=src.fnames)

#Prepare a set of remapping weights
# log_msg("Preparing weights...\n")
# remapping.wts <- file.path(remap.dir,PE.cfg$files$remapping.wts)
# wts.cmd <- cdo(csl("genbil",analysis.grid.fname),src.meta$fname[1],
#                         remapping.wts)

#'========================================================================
# Extract Fragments from Source Files ####
#'========================================================================
#Use the fragment meta data here as the trigging step. My fear is that
#an interruption of the source processing may lead to only a subset of
#the fragments being produced from a given file. Hence, required that
#the source extraction and fragment metadata are run in one large chunk
#all the way to completion.
frag.meta.fname <- file.path(chunk.dir,PE.cfg$files$fragment.meta)

#Loop over Source Files
log_msg("Extracting source files into fragments...\n")
if(!file.exists(frag.meta.fname) | pcfg@recalculate) {
  pb <- progress_estimated(nrow(src.meta),-1)
  for(i in seq(nrow(src.meta))) {
    #Show progress bar
    pb$print()
    
    #Extract file
    f <- src.meta$fname[i]
    log_msg("Extracting from %s...\n",basename(f),silenceable = TRUE)
    tmp.stem <- tempfile()
    
    #Subset out the layer(s) from the field of interest
    #log_msg("Select and remap...")
    if(!length(pcfg@vert.range)==0) {
      tmp.in <- f
      tmp.out <- sprintf("%s_sellevel",tmp.stem)
      vert.idxs <- verticalLayers(pcfg,this.chunk,tmp.in)
      sellev.cmd <- cdo(csl("sellevidx",vert.idxs),
                        tmp.in,tmp.out)
    } else {
      tmp.out <- f
    }

    #Average over the layers
    tmp.in <- tmp.out
    tmp.out <- sprintf("%s_vertmean",tmp.in)
    levmean.cmd <- cdo("vertmean",tmp.in,tmp.out)
    
    #Select the field of interest, just to be sure
    tmp.in <- tmp.out
    tmp.out <- sprintf("%s_selname",tmp.in)
    selname.cmd <- cdo(csl("selname",this.chunk@var),tmp.in,tmp.out)
    
    #Before selecting the months of interest, we may need to apply a time
    #correction of the time axis. CESM-DPLE, for example, has the time axis
    #set to 2018-08-01 to represent the period 2018-07-01-2018-08-01, meaning
    #that is actually the average value for July, but is labelled as August. It's a trap!
    #This is where we correct for that effect, and ensure that selmon works properly
    if(length(this.chunk@time.correction)!=0) {
      tmp.in <- tmp.out
      tmp.out <- sprintf("%s_timecorrect",tmp.in)
      shiftime.cmd <- cdo(csl("shifttime", this.chunk@time.correction),tmp.in,tmp.out)
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
    
    #Remap
    tmp.in <- tmp.out
    regrid.fname <- file.path(sel.dir,basename(f))
    
    #Remap Reusing weights
    # tmp.in <- tmp.out
    # regrid.fname <- file.path(sel.dir,basename(f))
    # regrid.cmd <- cdo("-f nc",
    #                   csl("remap", analysis.grid.fname, remapping.wts),
    #                   tmp.in, regrid.fname)
    
    #Remap recalculating weights every time
    tmp.in <- tmp.out
    regrid.fname <- file.path(sel.dir,basename(f))
    regrid.cmd <- cdo("-f nc",
                      csl("remapbil", analysis.grid.fname),
                      tmp.in, regrid.fname)
    
    #Fragment (split) into individual lead times
    #Note that the splitting indexing is something decided by cdo, not here, and
    #doesn't necessarily relate to the actual lead times in months/years.
    this.frag.fname <- file.path(frag.dir,gsub(".nc$","_L",basename(f)))
    frag.cmd <- cdo("splitsel,1",regrid.fname,this.frag.fname)
    
    #Remove the temporary files to tidy up
    tmp.fnames <- dir(dirname(tmp.stem),pattern=basename(tmp.stem),full.names = TRUE)
    del.err <- unlink(tmp.fnames)
    if(del.err!=0) stop("Error deleting temp files")
    
    #Update progress bar
    pb$tick()
 
  }
  pb$stop()
  print(pb)
  rm(pb)
  
  pcfg@recalculate <- TRUE   #If here, then force all subsequent calculations
  
  log_msg("\n")
}

#'========================================================================
# Collate fragment metadata  ####
#'========================================================================
#Get meta information 
log_msg("Collating meta information...\n")
if(!file.exists(frag.meta.fname) | pcfg@recalculate) {
  #Extract metadata from the files - this is controlled by the
  #various functions associated with the src model, and may
  #involving loading data from the file
  frag.fnames <- dir(frag.dir,pattern="*.nc",full.names = TRUE)
  frag.dates.l<- list()
  pb <- progress_estimated(length(frag.fnames))
  for(i in seq(frag.fnames)) {
    pb$tick()$print()
    log_msg("Collating metadata from fragment %s...\n",
            basename(frag.fnames[i]),silenceable = TRUE)
    
    frag.dates.l[[i]] <- this.chunk@date.fn(frag.fnames[i])
  }
  
  #Now build up a meta-data catalogue
  frag.meta <- tibble(src.name=this.chunk@name,
                      src.type=this.chunk@type,
                      start.date=this.chunk@init.fn(frag.fnames),
                      date=do.call(c,frag.dates.l),
                      lead.idx=str_match(basename(frag.fnames),"^.*?_L([0-9]+).nc$")[,2],
                      realization=this.chunk@realization.fn(frag.fnames),
                      fname=frag.fnames)
  saveRDS(frag.meta,file=frag.meta.fname)
  
  pb$stop()
  print(pb)
  rm(pb)
  log_msg("\n")

  pcfg@recalculate <- TRUE   #If here, then force all subsequent calculations
  
} else {
  frag.meta <- readRDS(frag.meta.fname)
}

#'========================================================================
# Generate fragment stacks ####
# Now we stack the fragments together to form 3D stacks, with each lon-lat
# layer corresponding to a realisation
#'========================================================================
log_msg("Building fragstacks...\n")
fragstack.meta.fname <- file.path(chunk.dir,PE.cfg$files$fragstack.meta)

if(!file.exists(fragstack.meta.fname)| pcfg@recalculate) {
  # Group data into the fragment stacks
  fragstack.grp <- split(frag.meta,
                         frag.meta[,c("date","lead.idx")],drop=TRUE)
  
  #Loop over groups and build the stacks
  pb <- progress_estimated(length(fragstack.grp))
  fragstack.meta.l <- list()
  for(i in seq(fragstack.grp)) {
    pb$tick()$print()
    grp <- fragstack.grp[[i]]
    log_msg("Building fragstack %i of %i...\n",i,
            length(fragstack.grp),silenceable = TRUE)
    
    #Stack
    fragstack.fname <- file.path(fragstack.dir,
                                 with(grp[1,],
                                      sprintf("%s_L%s_fragstack.nc",
                                              format(date,"%Y%m%d"),lead.idx)))
    # condexec(1,fragstack.cmd <- cdo("merge",
    #                                 grp$fname,
    #                                 fragstack.fname))
    fragstack.cmd <- ncecat("--rcd_nm realization -M",grp$fname,fragstack.fname)
    
    #Store metadata
    fragstack.meta.l[[i]] <- grp[1,] %>%
      mutate(#n.realizations=nrow(grp),
             fname=fragstack.fname)
    
  }
  Sys.sleep(0.1)
  print(pb$stop())
  log_msg("\n")
  
  #Save metadata
  fragstack.meta <- bind_rows(fragstack.meta.l) %>%
    select(-starts_with("realization")) 
  saveRDS(fragstack.meta,file=fragstack.meta.fname)

  pcfg@recalculate <- TRUE   #If here, then force all subsequent calculations
  
} else {
  fragstack.meta <- readRDS(fragstack.meta.fname)
}


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
