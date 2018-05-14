#/*##########################################################################*/
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

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Extract CMIP5 data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ClimateTools)
library(ncdf4)
load("objects/configuration.RData")


# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
 # mdl.no <- 5
  set.run.level(5)  #0 complete fresh run
} else {
  #Taking inputs from the system environment
#  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  #if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.run.level(0)  #0 complete fresh run
}

#Directory setup
src.dir <- file.path("data_srcs","CMIP5",pcfg@CMIP5.var)
base.dir <- define_dir("processing",pcfg@name,"CMIP5")
wts.dir <- define_dir(base.dir,"1.remapping_wts")
remap.dir <- define_dir(base.dir,"2.remap")
yearmean.dir <- define_dir(base.dir,"3.yearmean")
realmean.dir <- define_dir(base.dir,"4.realmean")
# realclim.dir <- define_dir(base.dir,"4.realclim")
clim.dir <- define_dir(base.dir,"5.clim")
anom.dir <- define_dir(base.dir,"6.anom")

#Check that climatology range is valid
if(any(pcfg@clim.years>2005))stop("Climatology years extend beyond 2005.")

# ========================================================================
# Setup
# ========================================================================
#Get list of files
fnames <- dir(src.dir,pattern=".nc",full.names = TRUE,recursive=TRUE)
if(length(fnames)==0) stop("Cannot find source files")

#Extract metadata
src.meta.df <- data.frame(fname=fnames)
src.meta.df$model <- CMIP5_model(src.meta.df$fname)
src.meta.df$expt <- CMIP5_experiment(src.meta.df$fname)
src.meta.df$real <- CMIP5_realisation(src.meta.df$fname)

#Setup filenames
src.meta.df$remap.fname <- file.path(remap.dir,basename(src.meta.df$fname))
src.meta.df$yearmean.fname <- with(src.meta.df,
                                sprintf("%s_%s_%s.nc",model,expt,real))
src.meta.df$yearmean.fname <- file.path(yearmean.dir,src.meta.df$yearmean.fname)
src.meta.df$realmean.fname <- with(src.meta.df,
                                   sprintf("%s_%s_realmean.nc",model,expt))
src.meta.df$realmean.fname <- file.path(realmean.dir,src.meta.df$realmean.fname)

#Split into models
mdl.l <- split(src.meta.df,src.meta.df$model)
# mdl.src.meta <- mdl.l[[mdl.no]]
mdl.src.meta <- src.meta.df

# ========================================================================
# Extract data
# ========================================================================
#Prepare a set of remapping weights
log_msg("Preparing weights...")
for(mdl in unique(mdl.src.meta$model)) {
  remapping.wts <- file.path(wts.dir,sprintf("%s_remapping_wts.nc",mdl))
  mdl.meta <- subset(mdl.src.meta,model==mdl)
  run_if(1,wts.cmd <- cdo(csl("genbil",pcfg@analysis.grid),
                          mdl.meta$fname[1],remapping.wts))
}

#Loop over files and extract the key data
for(i in seq(nrow(mdl.src.meta))) {
  #Add make-file like functionality by checking whether the final file exists
  #if(file.exists(mdl.src.meta$remap.fname[i])) next
  
  #Extract file
  f <- mdl.src.meta$fname[i]
  log_msg("Extracting from %s...\n",basename(f))
  
  #Subset out the surface layer from the field of interest
  #TODO: Note that we will probably need to change this in the future
  #to allow sub-surface properties - however, this is a bit of work
  #so we leave it for the moment.
  sellev.fname <- tempfile(fileext=".nc")
  run_if(2,sellev.cmd <- cdo("sellevidx,1",f,sellev.fname))
  
  #Select the field of interest, just to be sure
  selname.fname <- tempfile(fileext=".nc")
  run_if(2,selname.cmd <- cdo(csl("selname",pcfg@CMIP5.var),
                              sellev.fname,selname.fname))
  
  #Select the months of interest 
  selmon.fname <- tempfile(fileext=".nc")
  run_if(2,selmon.cmd <- cdo(csl("selmon", pcfg@MOI),
                              selname.fname,selmon.fname))
  
  #Remap
  remap.fname <- mdl.src.meta$remap.fname[i]
  remap.wts <- file.path(wts.dir,
                         sprintf("%s_remapping_wts.nc",mdl.src.meta$model[i]))
  run_if(2,remap.cmd <- cdo("-f nc", csl("remap", pcfg@analysis.grid, remap.wts),
                            selmon.fname, remap.fname))
  unlink(c(sellev.fname, selname.fname, selmon.fname))
}

# ========================================================================
# Merge files
# The CMIP5 files can be broken up into more manageable chunks - however,
# the breaking sometimes occurs in the middle of a year, rather than between
# years e.g. Had-CM3. This makes it hard to calculate averages that cross
# between two files. We solve this by first merging the files broken
# into individual files for a given realisation and experiment, and then
# apply the monthly averaging.
# ========================================================================
log_msg("Merging files...\n")
# Split into individual experiments and realisations
yearmean.l <- split(mdl.src.meta,mdl.src.meta$yearmean.fname)

#Now loop over the merging groups
for(ym in yearmean.l){
  log_msg("Merging to %s...\n",unique(ym$yearmean.fname))
  #Merge into single files
  merge.tmp.fname <- tempfile(fileext=".nc")
  merge.cmd <- cdo("-O mergetime",ym$remap.fname,merge.tmp.fname)
  run_if(3,merge.cmd)
  
  #Now perform averaging over the month
  yearmean.fname <- unique(ym$yearmean.fname)
  run_if(3,yearmean.cmd <- cdo("yearmean", merge.tmp.fname,yearmean.fname))
  unlink(merge.tmp.fname)
}

# ========================================================================
# Average over realisations
# ========================================================================
log_msg("Loading date metadata...\n")

#Some of the files are a bit corrupted and missing years - this creates
#all sorts of problems when trying to combine them into an ensemble mean
#We handle this situation by first identifying the files that are in the 
#majority, with respect to the number of timesteps - outliers from this
#are then excluded
if(run_if(4)) { #Only needed at run-level four, so skip otherwise
  #Using nc_open directly is double the speed
  mdl.src.meta$n.dates <- sapply(mdl.src.meta$yearmean.fname,
                                 function(f) {
                                   ncid <<- nc_open(f)
                                   n.dates <- ncid$dim$time$len
                                   nc_close(ncid)
                                   return(n.dates)})
  # Using raster
  # system.time(mdl.src.meta$n.dates <- sapply(mdl.src.meta$yearmean.fname,
  #                                function(f) length(getZ(brick(f)))))
  
  #CDO works well, but is surprisingly slow - probably due to the need for
  #system calls etc...
  # system.time(sapply(mdl.src.meta$yearmean.fname, function(f) {
  #         system(cdo("-s ntime",f),intern=TRUE)
  # }))
}
                                           

#Split it up for processing and loop
log_msg("Averaging over realisations...\n")
realmean.l <- split(mdl.src.meta,mdl.src.meta$realmean.fname)

for(rmean in realmean.l){
  #Get the date distributions for this data
  date.tbl <- table(rmean$n.dates)  
  if(length(date.tbl)>1) {  #Houston, we have a problem
    #Find the most common
    most.common <- names(date.tbl)[which.max(date.tbl)]
    #Find the problem children 
    prob.idx <- which(rmean$n.dates!=most.common)
    rmean <- rmean[-prob.idx,]
  }
    
  #Extract filenames
  yearmean.fnames <- file.path(yearmean.dir,unique(basename(rmean$yearmean.fname)))
  rmean.fname <- file.path(realmean.dir,unique(basename(rmean$realmean.fname)))
  
  #Calculate the realisation mean
  run_if(4,realmean.cmd <- cdo( "-O ensmean",yearmean.fnames,rmean.fname))

}

#These realisation means will form the basis of most of the rest of the
#processing, so we create a new metadata structure to store them
meta.df <- unique(mdl.src.meta[,c("realmean.fname","model","expt")])
meta.df$clim.fname <- file.path(clim.dir,
                                sprintf("%s_historical_clim.nc",meta.df$model))


# ========================================================================
# Calculate climatologies
# The climatology is a bit messy to calculate in this context, because of
# the fact that we have the historical experiments that end in 2005 and 
# then we split into multiple rcp experiments. We simplify this problem
# by focusing only on the historical experiment for the purpose of defining
# the climatology - we also make this redefinition through the rest of
# the configuration. This may cause some problems in the future, although
# it is hard to see at the moment where, because we have a comparison
# period as well, which is used for estimating skill metrics. Anyway,
# we simplify things by not allowing climatologies to extend beyond 2005
# and then proceed accordingly
# ========================================================================
log_msg("Calculating climatologies...\n")

#Now select the files to work with
hist.meta.df <- subset(meta.df,expt=="historical")

#Loop over historical files to calculate climatologies
for(i in seq(nrow(hist.meta.df))) {
  #Calculate the realisation climatology
  run_if(5,clim.cmd <- cdo("timmean",
                                csl("-selyear",pcfg@clim.years),
                                hist.meta.df$realmean.fname[i],
                                hist.meta.df$clim.fname[i]))
}

# ========================================================================
# Calculate anomalies 
# ========================================================================
log_msg("Anomalies...")

#Calculate the anomalies per realisation
for(m in seq(nrow(meta.df))){
  run_if(6,anom.cmd <- cdo("sub",
                           meta.df$realmean.fname[m],
                           meta.df$clim.fname[m],
                           file.path(anom.dir,
                                     basename(meta.df$realmean.fname)[m])))
}

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
