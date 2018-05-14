#/*##########################################################################*/
#' Calculate CDO Compatable Indicators
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Mon Jun  6 12:43:20 2016
#'
#' Calculates the various indicators for files that are compatable with CDO
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
cat(sprintf("\n%s\n","Calculate CDO indicators"))
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
#Take input arguments, if any
if(interactive()) {
  mdl.no <- 8
  options("run.level"= 0)  #0 complete fresh run
} else {
  #Taking inputs from the system environment
  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  options("run.level"= 0)  #0 complete fresh run
}

#Supported models
if(mdl.no==0) {
  mdl <- GCM(name="Ensmean",type="hindcast")
} else {
  mdl <- c(pcfg@hindcast.models,pcfg@uninit.models)[[mdl.no]]  
}


#Directory setup
base.dir <- define_dir("processing",pcfg@name)
obs.dir <- define_dir(base.dir,pcfg@observations[[1]]@name)
out.dir <- define_dir(base.dir,"indicators")

#Load metadata
load(file.path(base.dir,sprintf("%s-%s",mdl@name,mdl@type),"metadata.RData"))

# ========================================================================
# Setup 
# ========================================================================
#Setup observation data
obs.clim.fname <- file.path(obs.dir,"obs_climatology.nc")
obs.clim.full <- raster(obs.clim.fname)
obs.clim <- crop(obs.clim.full,pcfg@ROI)  #Crop down to size

#Setup landmask by regridding
landmask.fname <- file.path(base.dir,"landmask_regridded.nc")
run_if(NA,landmask.cmd <- cdo("-f nc", 
                              csl("remapnn", pcfg@analysis.grid),
                              pcfg@landmask, landmask.fname))
landmask <- raster(landmask.fname)

#Result storage
ind.l <- list()

# ========================================================================
# Calculate indicators per file 
# ========================================================================
#Loop over anomaly files
for(i in seq(nrow(meta.df))) {
  log_msg("Model %i, File %05i of %05i... ",mdl.no,i,nrow(meta.df))
  
  #Import model anom as a brick 
  f <- meta.df$fname[i]
  mdl.anom <- brick(f)
  
  #The resolutions of the observational climatology and the modelled anomaly match 
  #automatically, because an earlier step involves the interpolations of both the 
  #model output and observations onto the same analysis grid. This saves lots
  #of messing around with resolution adjustments etc
  
  #Note that differences in resolution can lead to differences in extent between
  #the products, even if they are extracted with the same basic ROI definition.
  #We therefore need to crop the data down to a common ROI before creating a
  #combined temperature object
  #mdl.temp <- obs.clim + crop(mdl.anom,pcfg@ROI)
  #Changes made where everything gets interpolated onto the same basic analysis
  #grid have largely rendered this step irrelevant
  
  #Build up the modelled temperature by combining the observational climatology
  #with the modelled anomaly.
  mdl.temp <- obs.clim + mdl.anom
  
  #Apply the land mask 
  log_msg("Masking... ")
  masked <- mask(mdl.temp,landmask,maskvalue=1)
  
  #Set the dates of the raster to be processed to be the same as those in the
  #original source file - these processing steps don't always propigate them
  #correctly
  masked <- setZ(masked,mdl@date_fn(f))

  # #Finally, drop layers that are completely blank
  # blank.layer <- cellStats(is.na(masked),sum)==ncell(masked)
  # no.blanks <- masked[[which(!blank.layer)]]
  
  #And we're ready. Lets calculate some indicators
  log_msg("indicators... ")
  ind.l[[i]] <- lapply(pcfg@indicators,eval.indicator,x=masked)
  ind.l[[i]] <- do.call(rbind,ind.l[[i]])
  ind.l[[i]]$fname <- f
  
  log_msg("Done.\n")
}

# ========================================================================
# Aggregate and tidy-up 
# ========================================================================
#Aggregate indicator list
mdl.inds.df <- do.call(rbind,ind.l)

#Merge in metadata as required
mdl.inds <- merge(mdl.inds.df,
                  meta.df[,c("fname","forecast.init","real")],
                  all=TRUE)
mdl.inds$fname <- NULL
mdl.inds$src <-  mdl@name
mdl.inds$type <- mdl@type

# #Average over realisations
# mdl.inds.ave.mat <- tapply(mdl.inds.r$value,
#                            mdl.inds.r[,c("indicator","forecast.init","date")],
#                           mean,drop=TRUE)
# mdl.inds.ave <- subset(melt(mdl.inds.ave.mat),!is.na(value))
# mdl.inds.ave$forecast.init <- ymd(mdl.inds.ave$forecast.init)
# mdl.inds.ave$date <- ymd(mdl.inds.ave$date)
# mdl.inds.ave$src <- class(mdl)
# mdl.inds.ave$real <- "realmean"

#Save indicators
save(mdl.inds,file=file.path(out.dir,
                             sprintf("%s_%s.RData",mdl@name,mdl@type)))

# ========================================================================
# Complete
# ========================================================================
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
