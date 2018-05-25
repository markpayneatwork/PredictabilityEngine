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
#' Extracts hindcast data from DCPP-like outputs and that is stored in a CDO compatable format
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
load("objects/setup.RData")
load("objects/configuration.RData")

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  src.no <- 2
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent()
  set.cdo.defaults("--silent --no_warnings -O")
} else {
  #Taking inputs from the system environment
  src.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(src.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything and tell us all about it
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent(FALSE)
  set.cdo.defaults()
}

#Supported models
src <- pcfg@decadal.hindcasts[[src.no]]

#Directory setup
src.dir <- file.path(datasrc.dir,src@source)
base.dir <- define_dir(pcfg@scratch.dir,src@source)
remap.dir <- define_dir(base.dir,"1.remapping_wts")
sel.dir <- define_dir(base.dir,"2.regrid")
frag.dir <- define_dir(base.dir,"3.fragments")
lead.clim.dir <- define_dir(base.dir,"4.lead.clims")
anom.dir <- define_dir(base.dir,"A.anom")
realmean.dir <- define_dir(base.dir,"B.realmean")

#'========================================================================
# Extract data ####
#'========================================================================
log_msg("Processing data source %s...\n",src@name)

#Get list of files
fnames <- dir(src.dir,pattern=".nc",full.names = TRUE)
if(length(fnames)==0 & get.debug.level()<=2) stop("Cannot find source files")

#Prepare a set of remapping weights
log_msg("Preparing weights...\n")
remapping.wts <- file.path(remap.dir,sprintf("%s_remapping_wts.nc",src@name))
condexec(1,wts.cmd <- cdo(csl("genbil",pcfg@analysis.grid),fnames[1],
                        remapping.wts))

#Loop over Files
log_msg("\nLooping over files...\n")
pb <- progress_estimated(length(fnames),-1)
for(i in seq(fnames)) {
  #Update progress bar
  pb$tick()$print()
  
  #Extract file
  f <- fnames[i]
  temp.stem <- tempfile()

  #Subset out the layer(s) from the field of interest
  #log_msg("Select and remap...")
  temp.in <- f
  temp.out <- sprintf("%s_sellevidx",temp.stem)
  condexec(2,sellev.cmd <- cdo(csl("sellevidx",src@levels),
                             temp.in,temp.out))
  
  #Average over the layers
  temp.in <- temp.out
  temp.out <- sprintf("%s_vertmean",temp.in)
  condexec(2,levmean.cmd <- cdo("vertmean",temp.in,temp.out))
  
  #Select the field of interest, just to be sure
  temp.in <- temp.out
  temp.out <- sprintf("%s_selname",temp.in)
  condexec(2,selname.cmd <- cdo(csl("selname",src@var),temp.in,temp.out))

  #Select the months of interest 
  temp.in <- temp.out
  temp.out <- sprintf("%s_selmon",temp.in)
  condexec(2,selmon.cmd <- cdo(csl("selmon", pcfg@MOI),
                               temp.in,temp.out))
  
  #Average over time - only necessary when considering multiple target months
  temp.in <- temp.out
  temp.out <- sprintf("%s_yearmean",temp.in)
  condexec(2,yearmean.cmd <- cdo( "yearmean", temp.in,temp.out))
  
  #Remap
  temp.in <- temp.out
  regrid.fname <- file.path(sel.dir,basename(f))
  condexec(2,regrid.cmd <- cdo("-f nc",
                               csl("remap", pcfg@analysis.grid, remapping.wts),
                               temp.in, regrid.fname))
  
  #Fragment (split) into individual lead times
  #Note that the splitting indexing is something decided by cdo, not here, and
  #doesn't necessarily relate to the actual lead times in months/years.
  this.frag.fname <- file.path(frag.dir,
                           gsub(".nc$","_L",basename(f)))
  condexec(3,frag.cmd <- cdo("splitsel,1",regrid.fname,this.frag.fname))

  #Remove the temporary files to tidy up
  tmp.fnames <- dir(dirname(temp.stem),pattern=basename(temp.stem),full.names = TRUE)
  del.err <- unlink(tmp.fnames)
  if(del.err!=0) stop("Error deleting temp files")
  
}
pb$stop()
print(pb)
rm(pb)
log_msg("\n")

#'========================================================================
# Collate fragment metadata  ####
#'========================================================================
#Get meta information 
log_msg("Collating meta information...\n")
if(get.debug.level()<=4) {
  #Extract metadata from the files - this is controlled by the
  #various functions associated with the src model, and may
  #involving loading data from the file
  frag.fnames <- dir(frag.dir,pattern="*.nc",full.names = TRUE)
  frag.dates.l<- list()
  pb <- progress_estimated(length(frag.fnames))
  for(i in seq(frag.fnames)) {
    pb$tick()$print()
    frag.dates.l[[i]] <- src@date_fn(frag.fnames[i])
  }
  
  #Now build up a meta-data catalogue
  frag.meta <- tibble(model=src@name,
                      start.date=src@init_fn(frag.fnames),
                      forecast.date=do.call(c,frag.dates.l),
                      lead.ts=str_match(basename(frag.fnames),"^.*?_L([0-9]+).nc$")[,2],
                      realization=src@ensmem_fn(frag.fnames),
                      fname=frag.fnames)
  save(frag.meta,file=file.path(base.dir,"Fragment_metadata.RData"))
  
  pb$stop()
  print(pb)
  rm(pb)
  log_msg("\n")
  
} else {
  load(file.path(base.dir,"Fragment_metadata.RData"))
}

#'========================================================================
# Calculate climatologies ####
#'========================================================================
log_msg("Calculating climatologies...\n")
#Break into chunks for climatology calculation
frag.meta$in.clim <- year(frag.meta$forecast.date) %in% pcfg@clim.years
lead.clim.files <- subset(frag.meta,in.clim)
lead.clim.files.l <- split(lead.clim.files,lead.clim.files$lead.ts)

#Calculate climatologies per lead
pb <- progress_estimated(length(lead.clim.files.l),-1)
for(l in names(lead.clim.files.l)) {
  pb$tick()$print()
  lf.df <- lead.clim.files.l[[l]]
  lead.clim.fname <- file.path(lead.clim.dir,
                               sprintf("%s_L%s_clim.nc",src@name,l))
  condexec(5,lead.clim.cmd <- cdo("-O ensmean",
                                  lf.df$fname,lead.clim.fname))
}

pb$stop()
print(pb)
rm(pb)
log_msg("\n")

#'========================================================================
# Calculate anomalies ####
#'========================================================================
log_msg("Calculating anomalies...\n")
#Simple loop over files
anom.meta <- mutate(frag.meta,
                    frag.fname=fname,
                    fname=file.path(anom.dir,
                                    sprintf("%s_S%s_L%s_%s_anom.nc",
                                            src@name,
                                            format(start.date,"%Y%m%d"),
                                            lead.ts,
                                            realization)),
                    clim.fname=file.path(lead.clim.dir,
                                         sprintf("%s_L%s_clim.nc",src@name,lead.ts)))
pb <- progress_estimated(nrow(anom.meta))
for(k in seq(nrow(anom.meta))){
  pb$tick()$print()
  condexec(6,anom.cmd <- cdo("sub",anom.meta$frag.fname[k],
                             anom.meta$clim.fname[k],
                             anom.meta$fname[k]))
}

pb$stop()
print(pb)
rm(pb)
log_msg("\n")

save(anom.meta,file=file.path(base.dir,"Anom_metadata.RData"))

#'========================================================================
# Calculate realisation means ####
#'========================================================================
log_msg("Realisation means...\n")
#Break into chunks per lead time and forecast date
realmean.meta <- mutate(anom.meta,
                        anom.fname=fname,
                        fname=file.path(realmean.dir,
                                                 str_replace(basename(anom.fname),
                                                             realization,
                                                             "realmean")))
realmean.files.l <- split(realmean.meta,
                          realmean.meta[,c("lead.ts","forecast.date")],
                          drop=TRUE)

#Average over the individual realisations at given lead time
pb <- progress_estimated(length(realmean.files.l),-1)
for(l in realmean.files.l) {
  pb$tick()$print()
  realmean.fname <- unique(l$fname)
  condexec(7,realmean.cmd <- cdo( "-O ensmean", l$anom.fname,realmean.fname))
}

pb$stop()
print(pb)
rm(pb)
log_msg("\n")

#Compile into metadata catalogue
realmean.meta <- mutate(realmean.meta,
                        realization="realmean") %>%
                 select(model,start.date,forecast.date,lead.ts,realization,fname)
save(realmean.meta,file=file.path(base.dir,"Realmean_metadata.RData"))

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
