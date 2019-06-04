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
library(tidyverse)
source("src/B.Configuration/B0.Define_common_data_srcs.r")

# ========================================================================
# Generic Configuration
# ========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "BW-Salinity",
                       MOI=3,  #March - ideally should be Feb-March-april average (??)
                       average.months=FALSE,
                       clim.years=1980:2010,  
                       comp.years=1970:2012,
                       landmask="data_srcs/NMME/landmask.nc")

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@use.global.ROI <- TRUE
pcfg@global.ROI <- extent(-25,0,40,65)
pcfg@global.res  <- 0.5

#Polygons
sp.objs <- list()
sp.objs$spawing.area <- spatial.domain("Spawning.area",extent(-20,-5,50,60))

#Correct names and add to object
names(sp.objs) <- sapply(sp.objs,slot,"name")
pcfg@spatial.subdomains <- sp.objs

#'========================================================================
# Data Sources ####
#'========================================================================
#Define observational sources
pcfg@Observations <- data.source(name="EN4",type="Observations",
                              var="salinity",
                              levels=17:23,
                              source=file.path("Observations","EN4"))

#Only one model to chose from here
pcfg@Decadal <- Sal.Decadal
pcfg@Decadal[["MPI-ESM-LR"]]@levels <- 13:19  #Set depth layers

#Check on CESM-DPLE vertical layers
# library(ncdf4)
# ncid <- nc_open(Sal.Decadal$`CESM-DPLE`@source[1])
# d <- data.frame(z_t=ncid$dim$z_t$vals,
#                 z_w=ncid$dim$z_w$vals,
#                 z_w_top=ncid$dim$z_w_top$vals,
#                 z_w_bot=ncid$dim$z_w_bot$vals) %>%
#       mutate_all(function(x){ x/ 100}) %>% #Convert to metres
#       add_column(.before=1,layer=seq(nrow(.)))
# filter(d,z_w_top>250,z_w_bot < 600)
pcfg@Decadal[["CESM-DPLE"]]@levels <- 24:35

#'========================================================================
# Statistics ####
#'========================================================================
#Configure stats
stat.l <- list()
stat.l[[1]]  <- spatial.mean(name="Mean Salinity")

stat.l[[2]] <- pass.through(name="Salinity field anomaly",
                               skill.metrics = "correlation",
                               is.global.stat=TRUE,
                               use.anomalies = TRUE,
                               use.realmeans=TRUE)

stat.l[[3]] <- area.threshold(name="Suitable spawning area",
                            skill.metrics = "correlation",
                            is.global.stat=FALSE,
                            use.anomalies = FALSE,
                            threshold=c(35.3,35.5),
                            use.realmeans=TRUE)

stat.l[[4]] <- threshold(name="Spawning suitability",
                              skill.metrics = "correlation",
                              is.global.stat=FALSE,
                              use.anomalies = FALSE,
                              threshold=c(35.3,35.5),
                              use.realmeans=TRUE)

#Merge it all in
names(stat.l) <- sapply(stat.l,slot,"name")
pcfg@statistics <- stat.l


#'========================================================================
# Output ####
#'========================================================================
source("src/B.Configuration/B99.Configuration_wrapup.r")

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
