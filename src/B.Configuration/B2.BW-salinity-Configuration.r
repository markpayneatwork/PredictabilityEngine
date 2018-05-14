#/*##########################################################################*/
#' Blue whiting salinity Configuration
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' TSun Sep  4 23:22:56 2016
#'
#' Configures a blue whiting salinity object
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
cat(sprintf("\n%s\n","Blue whiting salinity Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();

#Source the common elements
library(PredEng)

# ========================================================================
# Configuration
# ========================================================================
#Use the temperature set as a default
source("src/B.Configuration/B1.BW-temperature-Configuration.r")

#Correct name
pcfg@name <- "BW-Sal"

#Adjust observational sources and models to work with salinity instead of temperature
pcfg@observations[["EN4"]]@var <- "salinity"
pcfg@hindcast.models[["MPI-ESM-LR"]]@var <- "so"

#Update everything
pcfg <- update(pcfg)

# ========================================================================
# Output
# ========================================================================
#Write CDO grid descriptor
writeLines(griddes(pcfg),pcfg@analysis.grid)

#Output
save.image(file="objects/configuration.RData")
dmp <- define_dir(file.path("processing",pcfg@name))
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
