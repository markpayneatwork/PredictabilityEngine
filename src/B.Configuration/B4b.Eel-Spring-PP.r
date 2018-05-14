#/*##########################################################################*/
#' Eel Predictability Configuration - Spring Primary production
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Tue Jul 12 23:05:44 2016
#'
#' Defines a set of common-baseline elements for use across all pieces of code 
#' in this codebase - this case for the spawning of eel in spring (Feb-June)
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
cat(sprintf("\n%s\n","Eel - Spring PP Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();

#Source the common elements
library(PredEng)
library(ClimateTools)
# ========================================================================
# Configuration
# ========================================================================

#Use the spring Eel configruation as the template
source("src/B.Configuration/B4a.Eel-Spring.r")

#Rename it
pcfg@name <- "Eel-Spring-PP"

#Clear out the hindcast and uninit slots
pcfg@hindcast.models <- list()
pcfg@uninit.models <- list()
pcfg@observations <- list()

#Set CMIP5 variable accordingly
pcfg@CMIP5.var <- "intpp"

#Update
pcfg <- update(pcfg)

# ========================================================================
# Output
# ========================================================================
#Write CDO grid descriptor
pcfg@analysis.grid <- file.path("processing",pcfg@name,"analysis.grid")
writeLines(griddes(pcfg),pcfg@analysis.grid)

#Output
save.image(file="objects/configuration.RData")
save.image(file=file.path("processing",pcfg@name,"configuration.RData"))

# ========================================================================
# Done
# ========================================================================
#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete.\n")

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
