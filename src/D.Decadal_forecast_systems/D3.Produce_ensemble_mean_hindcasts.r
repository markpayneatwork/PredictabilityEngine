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
#' Produces ensemble mean hindcasts from the hindcast data
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
cat(sprintf("\n%s\n","Produce ensemble mean hindcasts"))
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
  options("run.level"= 1)  #0 complete fresh run
} else {
  #Taking inputs from the system environment
#  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
 # if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  options("run.level"= 0)  #0 complete fresh run
}

#Directory setup
base.dir <- define_dir("processing",pcfg@name)
ensmean.dir <- define_dir(base.dir,"Ensmean-hindcast")
nc.dir <- define_dir(ensmean.dir,"1.anoms")

# ========================================================================
# Setup ensemble averaging
# ========================================================================
#Start by loading the metadata associated with each of the hindcast
#models that we have in our configuration
metadat.l <- lapply(pcfg@hindcast.models,function(mdl){
                load(file.path(base.dir,sprintf("%s-hindcast",mdl@name),
                               "metadata.RData"))
                meta.df$src <- mdl@name
                return(meta.df)
})
metadat.all <- do.call(rbind,metadat.l)

#Now, start stripping out the files that we won't include in the ensemble mean
#The basic criteria is that they have to  represent the mean across realisations
#for that model. We could also apply criteria about having to have all models
#present and ensuring a uniform coverage of lead-times/forecast-dates, but 
#its probably easier just to let the user figure that aspect out. Cavet emptor.
metadat <- subset(metadat.all,real=="realmean") 

#Now split into groups by leadtime and forecast year
metadat$forecast.yr <- year(metadat$forecast.date)
grp.l <- split(metadat,metadat[,c("lead.ts","forecast.yr")],drop=TRUE)

# ========================================================================
# Perform averaging
# ========================================================================
#Setup somewhere to store metadat
ensmean.meta.l <- list()
#Loop over groupings
for(i in seq(grp.l)) {
  #Extract grouping
  log_msg("Processing %i of %i....\n",i,length(grp.l))
  d <- grp.l[[i]]
  
  #Build up meta data
  grp.meta <- data.frame(fname=NA,
                         forecast.date=mean(d$forecast.date),
                         forecast.init=mean(d$forecast.init),
                         lead.ts=unique(d$lead.ts), 
                         real="ensmean",src="Ensmean")
  grp.meta$fname <- file.path(nc.dir,
                              sprintf("Ensmean_%i_lead%s.nc",
                                      year(grp.meta$forecast.date),grp.meta$lead.ts))
                       
  #Average over individual files
  temp.fname <- tempfile(fileext = ".nc")
  run_if(1,ensmean.cmd <- cdo( "-O -ensmean", d$fname,temp.fname))
  
  #Set date
  run_if(2,date.cmd <- cdo(csl("setdate",format(grp.meta$forecast.date,"%Y-%m-%d")),
                            "-setreftime,1850-01-01,00:00:00,days",
                            "-setcalendar,proleptic_gregorian",
                           temp.fname,grp.meta$fname))
  unlink(temp.fname)
  
  #Store meta
  ensmean.meta.l[[i]] <- grp.meta
}

# ========================================================================
# Meta data handling
# ========================================================================
#Polish the anomaly file meta data into a more useable format
log_msg("Meta data handling...")
meta.df <- do.call(rbind,ensmean.meta.l)
save(meta.df,file=file.path(ensmean.dir,"metadata.RData"))

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
