#/*##########################################################################*/
#' Extract EN4 data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Fri Sep  2 14:13:44 2016
#'
#' Extracts EN4 data for subsequent metric analysis
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

#/*======================================================================*/
#  Initialise system
#/*======================================================================*/
cat(sprintf("\n%s\n","Extract EN4 data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ClimateTools)
load("objects/configuration.RData")

#/*======================================================================*/
#  Configuration
#/*======================================================================*/
#Take input arguments, if any
if(interactive()) {
  mdl.no <- 2
  set.run.level(1)  #1 complete fresh run
  
} else {
  #Taking inputs from the system environment
  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.run.level(1)  #1 complete fresh run
  
}

#Choose the data_src configuration
obs.srcs <- sapply(pcfg@observations,slot,"source")
if(!any(obs.srcs=="EN4")) stop("Not configured to use EN4 data")
dat.obj <- pcfg@observations[[which(obs.srcs=="EN4")[mdl.no]]]

#Working directories
src.dir <- file.path("data_srcs","EN4")
base.dir <- define_dir("processing",pcfg@name)
dat.dir <- define_dir(base.dir,dat.obj@name)
subset.dir <- define_dir(dat.dir,"1.subsetted")
unzip.dir <- tempdir()

#Contents of zipfiles
EN4.file.stem <- "EN.4.2.0.f.analysis.g10."


#/*======================================================================*/
#'## Extract EN4 data
#/*======================================================================*/
log_msg("Subsetting data...\n")

#First thing to do is to get metadata of the available files, and 
#use this to define future files
EN.meta <- data.frame(fname=dir(src.dir,pattern="*.zip",full.names = TRUE))
EN.meta$year <- as.numeric(str_match(basename(EN.meta$fname),
                                      "([0-9]{4}).zip$")[,2])
EN.meta$subset.fname <- file.path(subset.dir,gsub(".zip",".nc",
                                                   basename(EN.meta$fname)))

#Prepare a set of remapping weights based on decompressing one of these files
log_msg("Preparing weights...")

#Loop over Files
for(i in seq(nrow(EN.meta))) {
  #Extract file
  f <- EN.meta$fname[i]
  log_msg("%s...\n",basename(f))

  #Unzip the relevant files from the archive to the temp directory
  extract.files <- sprintf("%s%04i%02i.nc",
                           EN4.file.stem,
                           EN.meta$year[i],
                           pcfg@MOI)
  unzip.cmd <- ssl("unzip -o",f,
                     ssl(extract.files),sprintf("-d %s",unzip.dir))
  run_if(1,unzip.cmd)

  #For each individual file, strip out as much extra info as possible
  #by selecting the field of interest and the layers of interest
  selnames <- file.path(unzip.dir,sprintf("%s.select.nc",extract.files))
  for(j in seq(selnames) ) {
    #Select the levels and field of interest first
    sellev.fname <- tempfile(fileext=".nc")
    run_if(2,sellev.cmd <- cdo(csl("sellevidx",dat.obj@levels),
                               csl("-selname",dat.obj@var),
                               file.path(unzip.dir,extract.files[j]),
                               sellev.fname))
    
    #If we are dealing with temperature, need to convert from K to C
    if(dat.obj@var=="temperature") {
      next.fname <- sprintf("%s_degC.nc",sellev.fname)
      run_if(2,levmean.cmd <- cdo("addc,-273.15",sellev.fname,next.fname))
    } else {
      next.fname <- sellev.fname
    }
    
    #Average over the extracted levels
    run_if(2,levmean.cmd <- cdo("vertmean",next.fname,selnames[j]))
    
  }
  
  #Average over the files
  yearmean.fname <- tempfile(fileext=".nc")
  run_if(2,yearmean.cmd <- cdo( "ensmean",selnames,yearmean.fname))
  
  #Subset spatially
  run_if(2,remap.cmd <- cdo("-f nc", csl("sellonlatbox",as.vector(pcfg@ROI)),
                             yearmean.fname, EN.meta$subset.fname[i]))
  unlink(c(selnames, extract.files, yearmean.fname))
}

#Merge into a single file 
annave.fname<- file.path(dat.dir,"observations_native.nc")
run_if(3,annave.cmd <- cdo("-O -mergetime",
                           EN.meta$subset.fname,
                           annave.fname))

#Remap onto the analysis grid 
log_msg("Remapping...\n")
remap.fname <- file.path(dat.dir,"observations.nc")
run_if(4,remap.cmd <- cdo(csl("remapbil", pcfg@analysis.grid),
                           annave.fname, remap.fname))

#Calculate climatology
log_msg("Climatology....\n")
clim.fname <- file.path(dat.dir,"obs_climatology.nc")
run_if(5,clim.cmd <- cdo("timmean",
                         csl("-selyear",pcfg@clim.years),
                         remap.fname,clim.fname))

#Calculate anomalies
log_msg("Anomalies...\n")
anom.fname <- file.path(dat.dir,"obs_anom.nc")
run_if(6,anom.cmd <- cdo("sub",remap.fname,clim.fname,anom.fname))

# #Cross check that this worked corretly using Raster
# b.anom <- brick(anom.fname)
# b.clim <- brick(clim.fname)
# b.obs <- brick(annave.fname)
# b.obs.rec <- b.anom+b.clim  #Reconstructed
# b.diff <- b.obs - b.obs.rec
# #Check anomaly summation over the cliamtological period
# clim.yr.idxs <- which(year(getZ(b.anom)) %in% pcfg@clim.years)
# b.anom.sum <- sum(b.anom[[clim.yr.idxs]])

#/*======================================================================*/
#  Complete
#/*======================================================================*/
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
