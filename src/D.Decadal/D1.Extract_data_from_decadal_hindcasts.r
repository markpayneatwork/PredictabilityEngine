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
library(tibble)
library(dplyr)
library(ncdf4)
library(lubridate)
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
cfg.fname <- file.path(PE.cfg$dirs$job.cfg,"Decadal_Chunks.cfg")
this.cfgs <- get.this.cfgs(cfg.fname)
this.sp <- get.this.sp(cfg.fname,cfg.id,pcfg)
this.src <- get.this.src(cfg.fname,cfg.id,pcfg)
config.summary(pcfg,this.sp,this.src)

#Directory setup
subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
base.dir <- define_dir(subdomain.dir,"Decadal",this.src@name)
remap.dir <- define_dir(base.dir,"1.remapping_wts")
sel.dir <- define_dir(base.dir,"2.regrid")
frag.dir <- define_dir(base.dir,"3.fragments")
fragstack.dir <- define_dir(base.dir,"4.fragstacks")
analysis.grid.fname <- file.path(subdomain.dir,PE.cfg$files$analysis.grid)

#'========================================================================
# Setup ####
#'========================================================================
log_msg("Processing %s data source for %s subdomain ...\n",this.src@name,this.sp@name)

#Get list of files
src.fnames <- this.src@source
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
frag.meta.fname <- file.path(base.dir,PE.cfg$files$fragment.meta)

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
    if(!any(is.na(this.src@levels))) {
      tmp.in <- f
      tmp.out <- sprintf("%s_sellevidx",tmp.stem)
      sellev.cmd <- cdo(csl("sellevidx",this.src@levels),
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
    selname.cmd <- cdo(csl("selname",this.src@var),tmp.in,tmp.out)
    
    #Before selecting the months of interest, we may need to apply a time
    #correction of the time axis. CESM-DPLE, for example, has the time axis
    #set to 2018-08-01 to represent the period 2018-07-01-2018-08-01, meaning
    #that is actually the average value for July, but is labelled as August. It's a trap!
    #This is where we correct for that effect, and ensure that selmon works properly
    if(length(this.src@time.correction)!=0) {
      tmp.in <- tmp.out
      tmp.out <- sprintf("%s_timecorrect",tmp.in)
      shiftime.cmd <- cdo(csl("shifttime", this.src@time.correction),tmp.in,tmp.out)
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
    log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())
    
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
    
    frag.dates.l[[i]] <- this.src@date.fn(frag.fnames[i])
  }
  
  #Now build up a meta-data catalogue
  frag.meta <- tibble(src.name=this.src@name,
                      src.type=this.src@type,
                      start.date=this.src@init.fn(frag.fnames),
                      date=do.call(c,frag.dates.l),
                      lead.idx=str_match(basename(frag.fnames),"^.*?_L([0-9]+).nc$")[,2],
                      realization=this.src@realization.fn(frag.fnames),
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
fragstack.meta.fname <- file.path(base.dir,PE.cfg$files$fragstack.meta)

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
# Calculate climatologies ####
#'========================================================================
log_msg("Calculating climatologies...\n")

clim.meta.fname <-  file.path(base.dir,PE.cfg$files$clim.meta)

if(!file.exists(clim.meta.fname) | pcfg@recalculate) {
  
  #Break into chunks for climatology calculation by lead time and start month
  clim.meta <- mutate(fragstack.meta,
                      in.clim=year(date) %in% pcfg@clim.years,
                      start.month=month(start.date),
                      clim.idx=sprintf("S%02i.L%s",start.month,lead.idx),
                      clim.fname=file.path(lead.clim.dir,
                                           sprintf("%s_%s_clim.nc",
                                                   this.src@name,clim.idx)))
  rm(fragstack.meta)
  lead.clim.files <- subset(clim.meta,in.clim)
  lead.clim.files.l <- split(lead.clim.files,lead.clim.files$clim.idx)
  
  #Calculate climatologies per lead and start mnth
  pb <- progress_estimated(length(lead.clim.files.l),-1)
  for(l in names(lead.clim.files.l)) {
    pb$tick()$print()
    lf.df <- lead.clim.files.l[[l]]
    # lead.clim.fname <- file.path(lead.clim.dir,
    #                              sprintf("%s_L%s_clim.nc",this.src@name,l))
    # condexec(5,lead.clim.cmd <- cdo("-O ensmean",
    #                                 lf.df$fname,lead.clim.fname))
    
    #Calculate the climatology
    #We need to use nco here, rather than CDO - not sure why, but it
    #seems that the dimensionality is a bit too strange for CDO
    #First we do the averaging over all the individual dates to be included
    #in the climatology to product a climatology per realization. Then we need 
    #to average over the realization climatologies to get the total climatology
    #Note that we need to do the averaging with ncwa rather than ncra so 
    #anomaly creation via ncdiff works properly
    realclim.tmp <- tempfile()
    realclim.cmd <- nces(lf.df$fname,realclim.tmp)
    clim.cmd2 <- ncwa("-a realization", realclim.tmp,unique(lf.df$clim.fname))
    
  }
  
  pb$stop()
  print(pb)
  rm(pb)
  log_msg("\n")
  
  #Should probably tidy this metadata before saving, but as it is not used anyway, its not
  #all that critical
  saveRDS(clim.meta,file=clim.meta.fname)
  pcfg@recalculate <- TRUE   #If here, then force all subsequent calculations
  
} else {
  clim.meta <- readRDS(clim.meta.fname)
}

#'========================================================================
# Calculate anomalies ####
#'========================================================================
log_msg("Calculating anomalies...\n")
anom.meta.fname <- file.path(base.dir,PE.cfg$files$anom.meta)

if(!file.exists(anom.meta.fname) | pcfg@recalculate) {
  #Simple loop over files
  anom.meta <- dplyr::select(clim.meta,-in.clim,-clim.idx,-start.month) %>%
               mutate(fragstack.fname=fname,
                     fname=file.path(anom.dir,
                                     sprintf("%s_S%s_L%s_anom.nc",
                                             src.name,
                                             format(start.date,"%Y%m%d"),
                                             lead.idx)))
  pb <- progress_estimated(nrow(anom.meta))
  for(k in seq(nrow(anom.meta))){
    pb$tick()$print()
    log_msg("Calculating anomaly %s...\n",basename(anom.meta$fname[k]),silenceable = TRUE)
    # condexec(6,anom.cmd <- cdo("sub",anom.meta$fragstack.fname[k],
    #                            anom.meta$clim.fname[k],
    #                            anom.meta$fname[k]))
    anom.cmd <- ncdiff(anom.meta$fragstack.fname[k],
                       anom.meta$clim.fname[k],
                       anom.meta$fname[k])
  }
  
  pb$stop()
  print(pb)
  rm(pb)
  log_msg("\n")
  
  #Tidy up anom.meta for saving
  anom.meta <- select(anom.meta,-clim.fname,-fragstack.fname)
  
  saveRDS(anom.meta,file=anom.meta.fname)
  pcfg@recalculate <- TRUE   #If here, then force all subsequent calculations
} else {
  anom.meta <- readRDS(anom.meta.fname)
}

#'========================================================================
# Calculate realisation means ####
# This is easy now, as we can just average over the fragstacks
#'========================================================================
log_msg("Realisation means...\n")

realmean.meta.fname <- file.path(base.dir,PE.cfg$files$realmean.meta)

if(!file.exists(realmean.meta.fname) | pcfg@recalculate) {
  
  #Break into chunks per lead time and forecast date
  realmean.meta <- mutate(anom.meta,
                          anom.fname=fname,
                          fname=file.path(realmean.dir,
                                          str_replace(basename(anom.fname),
                                                      "anom",
                                                      "realmean")))
  
  #Average over the individual realisations at given lead time
  pb <- progress_estimated(nrow(realmean.meta),-1)
  for(i in seq(nrow(realmean.meta))) {
    pb$tick()$print()
    log_msg("Calculating realmean %s...\n",
            basename(realmean.meta$fname[i]),silenceable = TRUE)
    realmean.tmp <- tempfile()
    realmean.cmd <- ncra(realmean.meta$anom.fname[i],realmean.tmp)
    
    #Rename into a format suitable for combining into an ensmean
    realmean.tmp2 <- tempfile()
    rename.cmd <- ncrename(sprintf("-v %s,%s",this.src@var,PE.cfg$VOI.name),
                           realmean.tmp,realmean.tmp2)
    
    #And retain only this variable (and associated dimensions)
    drop.cmd <- ncks(sprintf("-v %s",PE.cfg$VOI.name),
                     realmean.tmp2,realmean.meta$fname[i])
    
    unlink(c(realmean.tmp,realmean.tmp2))
    
  }
  
  pb$stop()
  print(pb)
  rm(pb)
  log_msg("\n")
  
  #Compile into metadata catalogue by taking the first line in each groupling
  realmean.meta <- realmean.meta %>% 
    select(src.name,src.type,start.date,date,lead.idx,fname)
  saveRDS(realmean.meta,file=realmean.meta.fname)
  pcfg@recalculate <- TRUE   #If here, then force all subsequent calculations
} else {                                 
  realmean.meta <- readRDS(file=realmean.meta.fname)
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
