#/*##########################################################################*/
#' Setup system
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Sep  1 14:40:41 2016
#'
#' Builds and installs the package of support tools and sets some common
#' configuration parameters
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
cat(sprintf("\n%s\n","Build and install package"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

log_msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

#Do it 
install.packages("resources/ClimateTools", repos = NULL, type="source")
install.packages("resources/PredEng_Package", repos = NULL, type="source")

#/*======================================================================*/
#  Define common directories etc
#/*======================================================================*/
PE.cfg <- list()
PE.cfg$VOI.name <- "Variable_of_interest"
PE.cfg$ROI.extraction.buffer <- 2  #Degrees
PE.cfg$n.CMIP.chunks <- 5  #How many chunks should the CMIP5 ensemble be broken into?

#Directories
PE.cfg$dirs <- list(Misc.meta="Z.Misc.meta",
                    datasrc="data_srcs")

#File names
PE.cfg$files <- list(Obs.monthly.anom.metadata=file.path(PE.cfg$dir$Misc.meta,"Monthly_anom_metadata.RData"),
                     Obs.climatology.metadata=file.path(PE.cfg$dir$Misc.meta,"Climatology_metadata.RData"),
                     fragment.meta=file.path(PE.cfg$dir$Misc.meta,"Fragment_metadata.RData"),
                     fragstack.meta=file.path(PE.cfg$dir$Misc.meta,"Fragstack_metadata.RData"),
                     anom.meta="Anomaly_metadata.RData",
                     realmean.meta="Realmean_metadata.RData",
                     analysis.grid="analysis.grid",
                     remapping.wts="remapping_wts.nc",
                     ensmean.name="Ensmean",
                     regridded.landmask="Regridded_landmask.nc")

save(PE.cfg,file="objects/PredEng_config.RData")

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
