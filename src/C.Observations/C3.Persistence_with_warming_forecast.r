#/*##########################################################################*/
#' Persistence with Warming
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Fri Jul 15 20:50:04 2016
#'
#' Generates a persistence-with-warming forecast set with various lead times
#' and calculates the indicators on the results
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
cat(sprintf("\n%s\n","Persistence with warming"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")

#/*======================================================================*/
#  Configuration
#/*======================================================================*/
#Max lead
max.lead <- 10   #Years

#Directories
base.dir <- define_dir("processing",pcfg@name)
out.dir <- define_dir(base.dir,"indicators")
obs.dir <- define_dir(base.dir,pcfg@observations[[1]]@name)
src.obs <- file.path(obs.dir,"observations.nc")

options("run.level"= 0)  #0 complete fresh run

#/*======================================================================*/
#'## Setup forecast
#/*======================================================================*/
log_msg("Deriving warming trend...\n")
#Calculate the warming trend per pixel using CDO
trend.fname <- file.path(obs.dir,"trend.nc")
run_if(1,trend.cmd <- cdo("regres",
                          csl("-selyear",pcfg@comp.years),
                          src.obs, trend.fname))

#Setup support objects
obs.b <- brick(src.obs)
trend.b <- brick(trend.fname)

#/*======================================================================*/
#'## Generate indicators on forecast data
#/*======================================================================*/
pww.inds.l <- list()
for(l in seq(0,max.lead)) {
  log_msg("Generating indicators for lead %02i..\n",l)  
  #Add warming effect
  lead.b <- obs.b + trend.b*l
  #Adjust dates to produce a forecast object
  lead.b <- setZ(lead.b,getZ(obs.b)+years(l),"Date")
  
  #Apply indicators
  inds.l <- lapply(pcfg@indicators,eval.indicator,x=lead.b)
  inds <- do.call(rbind,inds.l)
  
  #Touch up object
  inds$src <- "PWW"
  inds$forecast.init <- inds$date - years(l)
  
  #Done
  pww.inds.l[[as.character(l)]] <- inds

}

#Save outputs
pww.inds <- do.call(rbind,pww.inds.l)
pww.inds$real <- NA
pww.inds$type <- "PWW"
save(pww.inds,file=file.path(out.dir,"PWW.RData"))

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
