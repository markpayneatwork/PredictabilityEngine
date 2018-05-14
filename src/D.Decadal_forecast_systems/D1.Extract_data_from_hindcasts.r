#/*##########################################################################*/
#' Extract CDO compatable data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jun  2 15:10:05 2016
#'
#' Extracts hindcast data that is stored in a CDO compatable format
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

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Extract CDO Compatable data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tibble)
library(dplyr)
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
  src.no <- 1
  set.run.level(4)  #0 complete fresh run
} else {
  #Taking inputs from the system environment
  src.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(src.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.run.level(0)  #0 complete fresh run
}

#Supported models
src <- pcfg@hindcast.models[[src.no]]

#Directory setup
src.dir <- file.path("data_srcs",src@src.dir)
base.dir <- define_dir(pcfg@scratch.dir,src@src.dir)
remap.dir <- define_dir(base.dir,"1.remapping_wts")
sel.dir <- define_dir(base.dir,"2.regrid")
split.dir <- define_dir(base.dir,"3.leads")
lead.clim.dir <- define_dir(base.dir,"4.lead.clims")
anom.dir <- define_dir(base.dir,"5.anom")
realmean.dir <- define_dir(base.dir,"6.realmean")

# ========================================================================
# Extract data
# ========================================================================
#Get list of files
fnames <- dir(src.dir,pattern=".nc",full.names = TRUE)
if(length(fnames)==0 & get.run.level()<=2) stop("Cannot find source files")

#Prepare a set of remapping weights
log_msg("Preparing weights...")
remapping.wts <- file.path(remap.dir,sprintf("%s_remapping_wts.nc",src@name))
condexec(1,wts.cmd <- cdo(csl("genbil",pcfg@analysis.grid),fnames[1],
                        remapping.wts))

#Loop over Files
for(i in seq(fnames)) {
  #Extract file
  f <- fnames[i]
  log_msg("%s...",basename(f))
  
  #Subset out the layer(s) from the field of interest
  log_msg("Select and remap...\n")
  sellev.fname <- tempfile(fileext=".nc")
  condexec(2,sellev.cmd <- cdo(csl("sellevidx",src@levels),
                             f,sellev.fname))
  
  #Average over the layers
  levmean.fname <- sprintf("%s_levmean.nc",sellev.fname)
  condexec(2,levmean.cmd <- cdo("vertmean",sellev.fname,levmean.fname))
  
  #Select the field of interest, just to be sure
  selname.fname <- tempfile(fileext=".nc")
  condexec(2,selname.cmd <- cdo(csl("selname",src@var),levmean.fname,selname.fname))

  #Select the months of interest 
  selmon.fname <- tempfile(fileext=".nc")
  condexec(2,selmon.cmd <- cdo(csl("selmon", pcfg@MOI),
                              selname.fname,selmon.fname))
  
  #Average over time
  yearmean.fname <- tempfile(fileext=".nc")
  condexec(2,yearmean.cmd <- cdo( "yearmean", selmon.fname,yearmean.fname))
  
  #Remap
  regrid.fname <- file.path(sel.dir,basename(f))
  condexec(2,regrid.cmd <- cdo("-f nc",
                               csl("remap", pcfg@analysis.grid, remapping.wts),
                               yearmean.fname, regrid.fname))
  unlink(c(sellev.fname, selname.fname, selmon.fname, yearmean.fname))

  #Split into individual lead times
  log_msg("Splitting...\n")
  split.fname <- file.path(split.dir,
                           gsub(".nc$","_lead",basename(f)))
  condexec(3,split.cmd <- cdo("splitsel,1",regrid.fname,split.fname))
  
}

# ========================================================================
# Calculate climatologies
# ========================================================================
#Get meta information 
log_msg("Collating meta information...\n")
lead.meta.df <- tibble(lead.fname=dir(split.dir,pattern="*.nc",full.names = TRUE))
lead.dates.l<- lapply(lead.meta.df$lead.fname,function(f) src@date_fn(f))
lead.meta.df$forecast.date <- do.call(c,lead.dates.l)
lead.meta.df$lead.ts <- str_match(basename(lead.meta.df$lead.fname),
                                     "^.*?lead([0-9]+).nc$")[,2]

#Break into chunks for climatology calculation
lead.meta.df$in.clim <- year(lead.meta.df$forecast.date) %in% pcfg@clim.years
lead.clim.files <- subset(lead.meta.df,in.clim)
lead.clim.files.l <- split(lead.clim.files,lead.clim.files$lead.ts)

#Calculate climatologies per lead
for(l in names(lead.clim.files.l)) {
  log_msg("Climatology for lead %s...\n",l)
  lf.df <- lead.clim.files.l[[l]]
  lead.clim.fname <- file.path(lead.clim.dir,
                               sprintf("clim_lead%s.nc",l))
  condexec(4,lead.clim.cmd <- cdo("-O ensmean",
                                  lf.df$lead.fname,lead.clim.fname))
}

# ========================================================================
# Calculate anomalies 
# ========================================================================
#Simple loop over files
lead.meta.df$anom.fname <- file.path(anom.dir,
                        gsub(".nc$","_anom.nc",basename(lead.meta.df$lead.fname)))
lead.meta.df$clim.fname <- file.path(lead.clim.dir,
                        sprintf("clim_lead%s.nc",lead.meta.df$lead.ts))
for(k in seq(nrow(lead.meta.df))){
  condexec(5,anom.cmd <- cdo("sub",lead.meta.df$lead.fname[k],
                           lead.meta.df$clim.fname[k],
                           lead.meta.df$anom.fname[k]))
}

# ========================================================================
# Calculate realisation means 
# ========================================================================
log_msg("Realisation means...\n")
#Break into chunks per lead time and forecast date
realmean.files.l <- split(lead.meta.df,
                          lead.meta.df[,c("lead.ts","forecast.date")],
                          drop=TRUE)

#Average over the individual realisations at given lead time
for(l in realmean.files.l) {
  realmean.fname <- gsub(src@ensmem_fn(l$anom.fname[1]),
                         "realmean",basename(l$anom.fname[1]))
  condexec(6,realmean.cmd <- cdo( "-O ensmean", l$anom.fname,
                                file.path(realmean.dir,realmean.fname)))
}

# ========================================================================
# Meta data handling
# ========================================================================
#Extract meta data for the realisation means
log_msg("Meta data handling...")
realmean.meta.df <- tibble(fname=dir(realmean.dir,pattern="*.nc",
                                         full.names = TRUE))
realmean.dates.l<- lapply(realmean.meta.df$fname,function(f) src@date_fn(f))
realmean.meta.df$forecast.date <- do.call(c,realmean.dates.l)
realmean.meta.df$lead.ts <- str_match(basename(realmean.meta.df$fname),
                             "^.*?lead([0-9]+)_anom.nc$")[,2]
realmean.meta.df$real <- "realmean"

#Polish the anomaly file meta data into a more useable format
meta.df <- mutate(lead.meta.df,fname=anom.fname,
                     real=src@ensmem_fn(lead.meta.df$anom.fname))
meta.df <- rbind(meta.df[,colnames(realmean.meta.df)],realmean.meta.df)
meta.df$forecast.init <- src@init_fn(meta.df$fname)
save(meta.df,file=file.path(base.dir,"metadata.RData"))

# ========================================================================
# Complete
# ========================================================================
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
