#/*##########################################################################*/
#' Extract data from uninitialised runs using CDO
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Mon Aug 29 00:06:36 2016
#'
#' Extracts data from uninitialised model runs using CDO
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
cat(sprintf("\n%s\n","Extract Uninitialised data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")
library(ncdf4)

# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
  mdl.no <- 1
  options("run.level"= 0)  #0 complete fresh run
} else {
  #Taking inputs from the system environment
  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  options("run.level"= 0)  #0 complete fresh run
}

#Supported models
mdl <- pcfg@uninit.models[[mdl.no]]

#Directory setup
src.dir <- file.path("data_srcs",mdl@name,"uninit")
base.dir <- define_dir("processing",pcfg@name,sprintf("%s-%s",mdl@name,mdl@type))
wts.dir <- define_dir(base.dir,"1.remapping_wts")
remap.dir <- define_dir(base.dir,"2.remap")
merge.dir <- define_dir(base.dir,"3.merged")
realclim.dir <- define_dir(base.dir,"4.realclim")
clim.dir <- define_dir(base.dir,"5.clim")
anom.dir <- define_dir(base.dir,"6.anom")
realmean.dir <- define_dir(base.dir,"7.realmean")

# ========================================================================
# Extract data
# ========================================================================
#Get list of files
fnames <- dir(src.dir,pattern=".nc",full.names = TRUE)
if(length(fnames)==0 & options("run.level")<=2) stop("Cannot find source files")

#Prepare a set of remapping weights
log_msg("Preparing weights...")
remapping.wts <- file.path(wts.dir,sprintf("%s_remapping_wts.nc",mdl@name))
run_if(1,wts.cmd <- cdo(csl("genbil",pcfg@analysis.grid),fnames[1],remapping.wts))

#Loop over files and extract the key data
for(i in seq(fnames)) {
  #Extract file
  f <- fnames[i]
  log_msg("%s...",basename(f))
  
  #Subset out the surface layer from the field of interest
  log_msg("Select and remap...\n")
  sellev.fname <- tempfile(fileext=".nc")
  run_if(2,sellev.cmd <- cdo("sellevidx,1",f,sellev.fname))
  
  #Select the field of interest, just to be sure
  selname.fname <- tempfile(fileext=".nc")
  run_if(2,selname.cmd <- cdo(csl("selname",mdl@var),sellev.fname,selname.fname))
  
  #Select the months of interest 
  selmon.fname <- tempfile(fileext=".nc")
  run_if(2,selmon.cmd <- cdo(csl("selmon", pcfg@MOI),
                              selname.fname,selmon.fname))
  
  #Average over the months of interest (if there are multiple)
  yearmean.fname <- tempfile(fileext=".nc")
  run_if(2,yearmean.cmd <- cdo( "yearmean", selmon.fname,yearmean.fname))
  
  #Remap
  remap.fname <- file.path(remap.dir,basename(f))
  run_if(2,remap.cmd <- cdo("-f nc", csl("remap", pcfg@analysis.grid, remapping.wts),
                             yearmean.fname, remap.fname))
  unlink(c(sellev.fname, selname.fname, selmon.fname, yearmean.fname))

}

# ========================================================================
# Combine files and process by realisation
# ========================================================================
#Get meta information 
log_msg("Collating meta information...\n")
remap.meta.df <- data.frame(fname=dir(remap.dir,pattern="*.nc",full.names = TRUE))
remap.meta.df$real <- mdl@ensmem_fn(remap.meta.df$fname)

#Split into realisations
real.l <- split(remap.meta.df,remap.meta.df$real)

#Loop over realisations
for(r in names(real.l)) {
  #Merge realisations together
  log_msg("Merging realisation %s...",r)
  real.merge.fname <- file.path(merge.dir,
                               sprintf("%s_%s.nc",mdl@name,r))
  run_if(3,merge.cmd <- cdo("-O mergetime",real.l[[r]]$fname,real.merge.fname))

  #Calculate the realisation climatology
  real.clim.fname <- file.path(realclim.dir,
                               sprintf("%s_realclim_%s.nc",mdl@name,r))
  run_if(4,real.clim.cmd <- cdo("timmean",
                                csl("-selyear",pcfg@clim.years),
                                real.merge.fname,
                                real.clim.fname))
}

# ========================================================================
# Calculate climatologies and anomalies 
# ========================================================================
log_msg("Climatologies and anomalies...")
#Average over the realisation climatologies to get the actual clim
real.clim.fnames <- dir(realclim.dir,pattern="*.nc",full.names = TRUE)
clim.fname <- file.path(clim.dir,sprintf("%s_clim.nc",mdl@name))
run_if(5,clim.cmd <- cdo("-O ensmean",real.clim.fnames,clim.fname))

#Need to first account for the fact that not all realisations have the 
#same length (potentially)
merged.fnames <- dir(merge.dir,pattern="*.nc",full.names=TRUE)
date.ranges.l <- lapply(merged.fnames,function(f) {
  d <- mdl@date_fn(f)
  return(data.frame(start=d[which.min(d)],
                    end=d[which.max(d)]))})
date.ranges <- do.call(rbind,date.ranges.l)
common.years <- year(max(date.ranges$start)):year(min(date.ranges$end))

#Calculate the anomalies per realisation
anom.fnames <- file.path(anom.dir,gsub(".nc$","_anom.nc",basename(merged.fnames)))
for(m in seq(merged.fnames)){
  run_if(6,anom.cmd <- cdo("sub",
                           csl("-selyear",common.years),merged.fnames[m],
                           clim.fname,anom.fnames[m]))
}

# ========================================================================
# Calculate realisation means 
# ========================================================================
log_msg("Realisation means...\n")

#Average over the individual realisations
realmean.fname <- file.path(realmean.dir,
                            sprintf("%s_realmean_anom.nc",mdl@name))
run_if(7,realmean.cmd <- cdo( "-O ensmean",anom.fnames,realmean.fname))

#Setup meta data
meta.df <- data.frame(fname=c(anom.fnames,realmean.fname))
meta.df$forecast.init <- as.Date(ISOdate(1850,1,1))
meta.df$real <- str_match(basename(meta.df$fname),"^.*_(.*?)_anom.nc$")[,2]
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
