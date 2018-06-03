#/*##########################################################################*/
#' Produce ensemble mean hindcasts
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Sat Sep  3 15:14:02 2016
#'
#' Produces ensemble mean hindcasts from the decadal hindcast data
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
cat(sprintf("\n%s\n","Produce ensemble mean hindcasts"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
library(tibble)
library(lubridate)
load("objects/configuration.RData")

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent()
  set.cdo.defaults("--silent -O --no_warnings")
} else {
  #Taking inputs from the system environment
#  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
 # if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}

#Ensemble mean source
src.types <- sapply(pcfg@decadal.hindcasts,class)
ensmean.src <- pcfg@decadal.hindcasts[[which(src.types=="data.ensemble")]]

#Directory setup
base.dir <- define_dir(pcfg@scratch.dir,"Decadal")
ensmean.dir <- define_dir(base.dir,ensmean.src@name,"C.ensmean")

#'========================================================================
# Setup ensemble averaging ####
#'========================================================================
#Start by loading the metadata associated with each of the hindcast
#models that we have chosen to use in our ensmean configuration
metadat.l <- list()
for(m in ensmean.src@members){
  if(class(m)=="data.source") {
    load(file.path(base.dir,m@name,"Realmean_metadata.RData"))
    metadat.l[[m@name]] <- realmean.meta
  }
}
metadat.all <- bind_rows(metadat.l)

#Now, start stripping out the files that we won't include in the ensemble mean
#The basic criteria is that they have to  represent the mean across realisations
#for that model. We could also apply criteria about having to have all models
#present and ensuring a uniform coverage of lead-times/forecast-dates, but 
#its probably easier just to let the user figure that aspect out. Cavet emptor.
#NB: This is a bit of a hangover from previous versions of the script where 
#we have all of the metadata lumped together. This is no longer necesssary. We 
#retain it here mainly for the note above.
metadat <- subset(metadat.all,realization=="realmean") 

#Now split into groups by leadtime and forecast year
#We could do the split directly on the date, but this is a bit
#risky - different models tend to handle and leap-years differently, so the
#date for one model might be 1964.08.15 and for another 1964.08.16
#even though they both represent the same thing.
metadat$forecast.yr <- year(metadat$date)
grp.l <- split(metadat,metadat[,c("lead.ts","forecast.yr")],drop=TRUE)

#'========================================================================
# Perform averaging ####
#'========================================================================
#Setup somewhere to store metadat
ensmean.meta.l <- list()
pb <- progress_estimated(length(grp.l))
log_msg("Performing averaging...\n")
#Loop over groupings
for(i in seq(grp.l)) {
  pb$tick()$print()

  #Extract grouping
  d <- grp.l[[i]]
  
  #Build up meta data
  grp.meta <- tibble(name=ensmean.src@name,
                         date=mean(d$date),
                         start.date=mean(d$start.date),
                         lead.ts=unique(d$lead.ts), 
                         realization="ensmean")
  ensmean.fname <- sprintf("%s_S%s_L%s_ensmean_anom.nc",
                           ensmean.src@name,
                           format(grp.meta$start.date,"%Y%m%d"),
                           grp.meta$lead.ts)
  grp.meta$fname <- file.path(ensmean.dir,ensmean.fname)

  #Average over individual files
  temp.fname <- tempfile(fileext = ".nc")
  condexec(1,ensmean.cmd <- cdo( "-O -ensmean", d$fname,temp.fname))
  
  #Set date
  condexec(2,date.cmd <- cdo(csl("setdate",
                                 format(grp.meta$date,"%Y-%m-%d")),
                                "-setreftime,1850-01-01,00:00:00,days",
                                "-setcalendar,proleptic_gregorian",
                               temp.fname,grp.meta$fname))
  unlink(temp.fname)
  
  #Store meta
  ensmean.meta.l[[i]] <- grp.meta
}

#Polish the anomaly file meta data into a more useable format
ensmean.meta <- bind_rows(ensmean.meta.l)
save(ensmean.meta,file=file.path(base.dir,"Ensmean_metadata.RData"))

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
