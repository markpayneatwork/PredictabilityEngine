#/*##########################################################################*/
#' Extract month of interest
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Mon May 23 10:45:27 2016
#'
#' Performs averaging across ensemble members and then extracts data for the
#' month of interest (MOI). Climatologies and anomalies are calculated thereafter.
#' This needs to be done in a single script to take advantage of the presence
#' of the lead-time information etc
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
cat(sprintf("\n%s\n","Extract Month of Interest Data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
#Helper functions, externals and libraries
load("objects/common_elements.RData")

library(reshape2)
library(stringr)
library(ncdf4)
library(raster)
library(lubridate)

# ========================================================================
# Configuration
# ========================================================================
base.dir <- "data/NMME"
download.dir <- file.path(base.dir,"2.downloads")
mean.dir  <- file.path(base.dir,"3.means")
MOI.dir  <- file.path(base.dir,"4.MoI")
clim.dir  <- file.path(base.dir,"5.clim")
anom.dir  <- file.path(base.dir,"6.anom")

options("run.level"= 1)  #0 complete fresh run

# ========================================================================
# Perform averaging over the ensemble
# ========================================================================
#Get downloaded files
file.df <- data.frame(download=dir(download.dir,pattern="nc$",full.names = TRUE))

#Setup prefixes
file.df$prefix <- gsub(".nc$","",basename(file.df$download))
file.df$mean <- file.path(mean.dir,sprintf("%s_modelmean.nc",file.df$prefix))
file.df$MOI <- file.path(MOI.dir,sprintf("%s_MoI.nc",file.df$prefix))
file.df$clim <- file.path(clim.dir,sprintf("%s_clim.nc",file.df$prefix))
file.df$anom <- file.path(anom.dir,sprintf("%s_anom.nc",file.df$prefix))

# ========================================================================
#  Processing
# ========================================================================
#Loop over Files
for(i in seq(nrow(file.df))) {
  #First, the NASA model contains a member with a very limited number
  #of starts. This needs to be removed
  log_msg("\nWorking with %s... ",file.df$prefix[i])
  if(grepl("NASA-GEOS5",file.df$prefix[i])) {
    clean.fname <- file.path(tempdir(),"NASA_GEOS5.nc")
    clean.cmd <- paste("ncks -O -F -D1 -d M,1,11",
                       file.df$download[i],clean.fname)
    run_if(1,clean.cmd)
    file.df$download[i] <- clean.fname
    }
  
  #Perform averaging with ncwa
  log_msg("Averaging... ", file.df$prefix[i])
  mean.fname <- file.df$mean[i]
  mean.cmd <- paste("ncwa -O -D1 -a M",
                      file.df$download[i],mean.fname)
  run_if(1,mean.cmd)


  #Open file and identify extraction months
  log_msg("\nLoading...")
  ncid <- nc_open(mean.fname)
  start.grd <- expand.grid(L=ncid$dim$L$vals,S=ncid$dim$S$vals)
  if(!identical("months since 1960-01-01",ncid$dim$S$units)) stop("Time units mismatch")
  start.grd$start.date <- ISOdate(1960,01,01) + months(start.grd$S)
  start.grd$forecast.mon <- start.grd$S + start.grd$L
  start.grd$forecast.date <- ISOdate(1960,01,1) + months(floor(start.grd$forecast.mon))
  start.grd$forecast.month <- month(start.grd$forecast.date)
  sel.MoI <- subset(start.grd,forecast.month %in% month.interest)
  sel.MoI$L.idx <- as.numeric(factor(sel.MoI$L,levels = ncid$dim$L$vals))
  sel.MoI$S.idx <- as.numeric(factor(sel.MoI$S,levels = ncid$dim$S$vals))
  
  #Extract data 
  log_msg("Extracting month %i data... ",month.interest)
  res.l <- list()
  for(j in seq(nrow(sel.MoI))) {
    d <- ncvar_get(ncid,"sst",count=c(-1,-1,1,1),
                               start=c(1,1,sel.MoI$L.idx[j],sel.MoI$S.idx[j]))
    res.l[[j]] <- r <- raster(a<-list(x=ncid$dim$X$vals,y=ncid$dim$Y$vals,z=d),crs=ll.crs)
  }
  
  #Write out results
  log_msg("Saving... ")
  MoI.b <- brick(res.l)
  MoI.b <- setZ(MoI.b, as.Date(sel.MoI$start.date),"Date")
  writeRaster(MoI.b,format="CDF",overwrite=TRUE,filename = file.df$MOI[i],
              varname="SST",zname="time")
  nc_close(ncid)
  
  #Setup for climatologies
  log_msg("Climatologies... ")
  sel.MoI$forecast.year <- year(sel.MoI$forecast.date)
  sel.MoI$in.clim <- sel.MoI$forecast.year %in% clim.yrs
  sel.MoI$idx <- seq(nrow(sel.MoI))
  clim.meta <- subset(sel.MoI,in.clim)
  clim.meta.l <- split(clim.meta,clim.meta$L.idx)

  #Calculate climatologies at different lead times
  clim.lead.l <- lapply(clim.meta.l,function(m) {
       sel.b <- MoI.b[[m$idx]]
       mean.b <- mean(sel.b)
       return(mean.b)
  })
  clim.b <- brick(clim.lead.l)
  clim.b <- setZ(clim.b,as.numeric(names(clim.lead.l))-1,"lead")
  writeRaster(clim.b,format="CDF",overwrite=TRUE,filename=file.df$clim[i],
              varname="SST",zname="lead",zunit="months")
  
  #Calculate the anomalies using the climatologies defined at appropriate
  #lead times.
  log_msg("Anomalies.... ")
  anom.b <- MoI.b - clim.b[[sel.MoI$L.idx]]
  anom.b <- setZ(anom.b,getZ(MoI.b))
  writeRaster(anom.b,format="CDF",overwrite=TRUE,filename=file.df$anom[i],
              varname="SST_anom",zname="time")
  log_msg("Done.\n")
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
