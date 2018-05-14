#/*##########################################################################*/
#' Eel Predictability Configuration
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Tue Jul 12 23:05:44 2016
#'
#' Defines a set of common-baseline elements for use across all pieces of code 
#' in this codebase
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
cat(sprintf("\n%s\n","Eel Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();

#Source the common elements
library(PredEng)
library(ClimateTools)
source("src/B.Configuration/B0.Define_SST_data_srcs.r")

# ========================================================================
# Configuration
# ========================================================================
#Global project configuration
pcfg <- config(name= "Eel",
               res=0.25,
               MOI=1:12,  
               clim.years=1960:2005,    #2005 endpoint corrsponds to CMIP5
               comp.years=1960:2010,    
               CMIP5.var="tos",
               landmask="data_srcs/NMME/landmask.nc",
               observations=SST_obs[c("EN4","HadISST")],
               uninit.models=uninit_mdls)

#Time series bbox
inds <- list()
inds[["area"]] <- new("spatial.mean",
                      name="Mean Temp. (degC)",
              poly.ROI=as(extent(c(-66,-50,22,32)),"SpatialPolygons"))

#Exclude the MPI-NCEP forced model
pcfg@hindcast.models <- hindcast_mdls[which(names(hindcast_mdls)!="MPI-NCEP")]

#Add final touches
pcfg@indicators <- inds
pcfg@analysis.grid <- file.path("processing",pcfg@name,"analysis.grid")
rm(inds)

#When subsetting data using eg THREDDS or OpenDAP, we want to download
#some extract padding anyway, just to be sure. Here we do that
pcfg@ROI <- extend(merge(pcfg),2)

#Update everything
pcfg <- update(pcfg)

# ========================================================================
# Output
# ========================================================================
#Write CDO grid descriptor
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
