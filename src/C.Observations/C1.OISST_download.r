#/*##########################################################################*/
#' Download and Build OISST data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Fri May 20 10:04:32 2016
#'
#' Downloads OISST data to serve as a reference set of observations
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
cat(sprintf("\n%s\n","Download OISST Data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)


#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
base.dir <- file.path("processing",pcfg@name)
dat.dir <- file.path(base.dir,"OISST")
src.url <- "http://www.ncdc.noaa.gov/thredds/dodsC/OISST-V2-AVHRR_agg_combined"
download.yrs <- 1982:2015
obs.clim.fname <- file.path(dat.dir,"obs_climatology.nc")

set.debug.level(0)  #0complete fresh run, #5 to redefine the climatology years

#Adjust the configurations to lineup with the [0,360] grid range of OISST
pcfg <- rotate(pcfg)  

# ========================================================================
# Retrieve data using OpenDAP
# ========================================================================
#Build ROI selection command
ROI.str <- do.call(sprintf,c("-d lon,%.2f,%.2f -d lat,%.2f,%.2f",
                         as.list(as.vector(pcfg@ROI))))

#If doing a clean run, remove directories etc
if(get.debug.level()<=1) {
  unlink(dat.dir,recursive = TRUE,force=TRUE)
  dir.create(dat.dir)
  dir.create(file.path(dat.dir,"daily"))
  dir.create(file.path(dat.dir,"monthly"))
}

#Loop over individual years
for(yr in download.yrs) {
  log_msg("\nNow downloading %i data...\n",yr)
  
  #Build time selection command
  #Time for OISST is expressed as days since 1978-01-01 00:00:00
  ref.time.str <- "1978-01-01 12:00:00"  #Makes it easier
  ref.time <- ymd_hms(ref.time.str)
  t.ROI.start <-ISOdate(yr,min(pcfg@MOI),1)
  t.ROI.end <-ISOdate(yr,max(pcfg@MOI),days_in_month(max(pcfg@MOI)))  #Last day of month
  t.ROI.str <- sprintf("-d time,%.2f,%.2f",
                       difftime(t.ROI.start,ref.time,"days"),
                       difftime(t.ROI.end,ref.time,"days"))
  
  # #Now do the retrievalwith ncks and OpenDAP
  download.fname <- file.path(dat.dir,"daily",sprintf("%i.nc",yr))
  run_if(1, download.cmd <-ncks("-O -D 1 --netcdf4 -v sst",ROI.str,t.ROI.str, 
                        src.url,download.fname))

  #Perform averaging with ncwa
  ave.fname <- file.path(dat.dir,"monthly",sprintf("%i_monthly.nc",yr))
  run_if(2,ave.cmd <- ncwa("-O -D1 -a time --retain-degenerate-dimensions",
                   download.fname,ave.fname))

  #Make a record dimension
  run_if(3,record.cmd <- ncks("--mk_rec_dmn time -O",ave.fname,ave.fname))
}

#Build a single file
log_msg("\nBuilding a single file...\n")
mon.fnames <- dir(file.path(dat.dir,"monthly"),full.names = TRUE,
                  pattern="^[0-9]{4}_monthly.nc$")
build.fname <- file.path(dat.dir,"observations_native.nc")
run_if(4,build.cmd <- ncrcat("-D 1 -O -o",build.fname,mon.fnames))

#Remap onto the analysis grid
log_msg("Remapping...\n")
remap.fname <- file.path(dat.dir,"observations.nc")
run_if(5,remap.cmd <- cdo("-f nc", csl("remapbil", pcfg@analysis.grid),
                          build.fname, remap.fname))

#Calculate climatology
log_msg("\nCalculate climatology and anomalies...\n")
OISST.b <- brick(remap.fname)
clim.idx <- which(year(getZ(OISST.b)) %in% pcfg@clim.years)
OISST.clim <- mean(OISST.b[[clim.idx]])
OISST.anom <- OISST.b - OISST.clim 
OISST.anom <- setZ(OISST.anom,getZ(OISST.b),"Date")
writeRaster(OISST.clim,format="CDF",overwrite=TRUE,
            varname="SST",
            filename =  obs.clim.fname)
writeRaster(OISST.anom,format="CDF",overwrite=TRUE,
            filename =  file.path(dat.dir,"obs_anom.nc"))

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
