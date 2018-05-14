#/*##########################################################################*/
#' Calculate Observational indicators
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jul 14 14:37:03 2016
#'
#' Applies project-level indicators to the observational data sets
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
cat(sprintf("\n%s\n","Calculate Observational indicators"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")

library(reshape2)
library(ggplot2)

# ========================================================================
# Setup
# ========================================================================
#Directories
base.dir <- define_dir("processing",pcfg@name)
out.dir <- define_dir(base.dir,"indicators")

# ========================================================================
# Loop over data sets and apply indicators
# ========================================================================
for(obs.dat.set in pcfg@observations) {
  log_msg("Processing %s...\n",obs.dat.set@name)
  
  #Setup source data brick
  obs.b <- brick(file.path(base.dir,obs.dat.set@name,"observations.nc"))
  
  #Apply indicators
  obs.inds.l <- lapply(pcfg@indicators,eval.indicator,x=obs.b)
  
  #Combine indicators
  obs.inds <- do.call(rbind,obs.inds.l)
  obs.inds$type <- "obs"
  obs.inds$src <- obs.dat.set@name
  obs.inds$forecast.init <- obs.inds$date
  obs.inds$real <- NA
  
  #Plot
  # g <- ggplot(obs.inds,aes(y=value,x=date))+geom_line()+geom_point()+
  #   facet_wrap(~indicator,scales = "free")
  # print(g)
  
  #Save outputs
  save(obs.inds,file=file.path(base.dir,"indicators",
                               sprintf("%s.RData",obs.dat.set@name)))
}

# ========================================================================
# Finish
# ========================================================================
#Turn off the lights
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
