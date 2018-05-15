#/*##########################################################################*/
#' Extract hindcast data and process
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jan 12 21:44:44 2017
#'
#' Extracts hindcast data that is stored in a CDO compatable format from multiple
#' hindcast data sources and then applies indicator calculations, all in one hit
#' This script is largely intended to be used with the LME predictability analysis
#' done with Desiree Tommasi at GFDL, and therefore will be rather less generic
#' than many of the other scripts. In particular, it is intended to be used 
#' so that it loops over configruations, rather than data sources, and there 
#' is essentially no bias correction.
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
cat(sprintf("\n%s\n","Extract data from hindcasts and process"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ClimateTools)

# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.no <- 44   #NZ Shelf
  set.debug.level(0)  #0 complete fresh run
} else {
  #Taking inputs from the system environment
  cfg.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(cfg.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}

#Load the configuration
LME.dirs <- dir("processing/LME/")
LME.dir  <- file.path("processing","LME",
                      LME.dirs[pmatch(sprintf("%02i",cfg.no),LME.dirs)])
load(file.path(LME.dir,"configuration.RData"))
log_msg("Preparing %s configuration....\n",pcfg@name)

#Misc config
out.dir <- define_dir("processing/LME/indicators")

# ========================================================================
# Setup model 
# ========================================================================
#Setup looping over models
for(mdl in pcfg@hindcast.models) {

#Configure directories
src.dir <- file.path("data_srcs",mdl@name,"hindcasts")
base.dir <- define_dir(LME.dir,mdl@name)
regrid.dir <- define_dir(base.dir,"1.regrid")
split.dir <- define_dir(base.dir,"2.split_by_leads")

#Get list of files
fnames <- dir(src.dir,pattern=".nc",full.names = TRUE)
if(length(fnames)==0 & get.debug.level()<=2) stop("Cannot find source files")

#Prepare a set of remapping weights
log_msg("Preparing weights...")
remapping.wts <- file.path(base.dir,sprintf("%s_remapping_wts.nc",mdl@name))
run_if(1,wts.cmd <- cdo(csl("genbil",pcfg@analysis.grid),fnames[1],remapping.wts))

# ========================================================================
# Extract data from each model
# ========================================================================
#Loop over Files
for(i in seq(fnames)) {
  #Extract file
  f <- fnames[i]
  log_msg("%s...",basename(f))
  
  #Subset out the layer(s) from the field of interest
  log_msg("Select and remap...\n")
  sellev.fname <- tempfile(fileext=".nc")
  run_if(2,sellev.cmd <- cdo(csl("sellevidx",mdl@levels),
                             f,sellev.fname))
  
  #Average over the layers
  levmean.fname <- sprintf("%s_levmean.nc",sellev.fname)
  run_if(2,levmean.cmd <- cdo("vertmean",sellev.fname,levmean.fname))
  
  #Select the field of interest, just to be sure
  selname.fname <- tempfile(fileext=".nc")
  run_if(2,selname.cmd <- cdo(csl("selname",mdl@var),levmean.fname,selname.fname))

  #Select the months of interest 
  selmon.fname <- tempfile(fileext=".nc")
  run_if(2,selmon.cmd <- cdo(csl("selmon", pcfg@MOI),
                              selname.fname,selmon.fname))
  
  #Average over time
  yearmean.fname <- tempfile(fileext=".nc")
  run_if(2,yearmean.cmd <- cdo( "yearmean", selmon.fname,yearmean.fname))
  
  #Regrid
  regrid.fname <- file.path(regrid.dir,basename(f))
  run_if(2,regrid.cmd <- cdo("-f nc", csl("remap", pcfg@analysis.grid, remapping.wts),
                             yearmean.fname, regrid.fname))
  unlink(c(sellev.fname, selname.fname, selmon.fname, yearmean.fname))

  #Split into individual lead times
  log_msg("Splitting...\n")
  split.fname <- file.path(split.dir,
                           gsub(".nc$","_lead",basename(f)))
  run_if(3,split.cmd <- cdo("splitsel,1",regrid.fname,split.fname))
}
 
# ========================================================================
# Calculate indicators
# ========================================================================
log_msg("Calculating indicators...\n")
#Setup for calculation
split.fnames <- dir(split.dir,full.names = TRUE)
ind.l <- list()

#Loop over files
for(i in seq(split.fnames)){
  f <- split.fnames[i]
  log_msg("%s.... indicators...\n",basename(f))
  #Import model anom as a brick 
  mdl.val <- raster(f)
  
  #Calculate indicators
  ind.l[[i]] <- lapply(pcfg@indicators,eval.indicator,x=mdl.val)
  ind.l[[i]] <- do.call(rbind,ind.l[[i]])
  ind.l[[i]]$fname <- basename(f)
}

#Aggregate indicator list and add meta-data
mdl.inds.df <- do.call(rbind,ind.l)
mdl.inds.df$init.date <- ymd(gsub("S","",underscore_field(mdl.inds.df$fname,5)  ))
mdl.inds.df$forecast.lead <- as.numeric(str_match(mdl.inds.df$fname,
                                              "^.*?lead([0-9]+).nc$")[,2])
mdl.inds.df$realisation <- underscore_field(mdl.inds.df$fname,6)


#Save results
out.df <- transform(mdl.inds.df, fname=NULL,indicator=NULL,forecast.period.mid.date=date,
                    date=NULL)
write.csv(out.df,file=file.path(out.dir,sprintf("%s#%s.csv",pcfg@name,mdl@name)),
          row.names = FALSE,quote=FALSE)

} #End model looping

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
