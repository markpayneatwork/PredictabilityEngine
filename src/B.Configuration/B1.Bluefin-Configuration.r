#/*##########################################################################*/
#' Bluefin Predictability Common Elements
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
cat(sprintf("\n%s\n","Bluefin Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();

#Source the common elements
library(PredEng)
load("objects/setup.RData")
source("src/B.Configuration/B0.Define_SST_data_srcs.r")

# ========================================================================
# Configuration
# ========================================================================
#Global project configuration
pcfg <- config(name= "Bluefin",
               ROI=extent(-70,30,50,80),
               res=0.5,
               MOI=8,  #August
               clim.years=1983:2010,  
               comp.years=1961:2012,
               landmask="data_srcs/NMME/landmask.nc",
               observations=SST_obs[c("HadISST")],
               CMIP5.var="tos",
               uninit.models=uninit_mdls,
               NMME.models=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@name)
define_dir(pcfg@scratch.dir)

#Drop NCEP forced model
pcfg@hindcast.models <- hindcast_mdls[-which(names(hindcast_mdls)=="MPI-NCEP")]

#Analysis grid
pcfg@analysis.grid <- file.path(pcfg@scratch.dir,"analysis.grid")

# ========================================================================
# Spatial Configurations
# ========================================================================
irminger.sea.sp <- as.SpatialPolygons.matrix(rbind(c(-45,58),c(-45,66),
                                                   c(-20,66),c(-32,58)))
iceland.basin.sp <- as.SpatialPolygons.matrix(rbind(c(-20,66),c(-32,58),
                                                    c(-15,58),c(-15,66)))
norwegian.coast.xy <- rbind(c(-5,62),c(10,62),c(20,70),c(20,73),c(12,73))
norwegian.coast.sp <- as.SpatialPolygons.matrix(norwegian.coast.xy)
south.iceland.sp <- as(extent(-50,-10,55,70),"SpatialPolygons")

# r <- crop(raster("~/Documents/common_data/ETOPO/ETOPO15_mean_bath.nc"),pcfg@ROI)
# image(r)
# plot(irminger.sea.sp,add=TRUE,border="white")
# plot(iceland.basin.sp,add=TRUE,border="white")
# plot(norwegian.coast.sp,add=TRUE,border="white")
# map("world",add=TRUE,fill=TRUE)
# ========================================================================
# Inidcator Configurations
# ========================================================================
#Area above temperature indicator
ind.l <- list()
ind.l$South.Iceland <- above.threshold(name="South of Iceland",
                                 threshold=11,
                                 poly.ROI=south.iceland.sp)

ind.l$IS.area <- above.threshold(name="Irminger Sea",
                                 threshold=11,
                                 poly.ROI=irminger.sea.sp)

ind.l$IB.area <- above.threshold(name="Iceland Basin",
                             threshold=11,
                             poly.ROI=iceland.basin.sp)
ind.l$norway.area <- above.threshold(name="Norwegian Coast",
                                     threshold=11,
                                     poly.ROI=norwegian.coast.sp)

#Time series
ind.l$irminger.mean <- spatial.mean(name="Irminger Sea",
              poly.ROI=irminger.sea.sp)
ind.l$iceland.mean <- spatial.mean(name="Iceland Basin",
              poly.ROI=iceland.basin.sp)
ind.l$norway.mean <- spatial.mean(name="Norwegian Coast",
                    poly.ROI=norwegian.coast.sp)

#Northward extent
ind.l$norway.north <-isoline.lat(name="Norwegian Coast",
                                    threshold=11,
                                    poly.ROI=norwegian.coast.sp)

#Merge it all in
pcfg@indicators <- ind.l

# ========================================================================
# Output
# ========================================================================
#Update everything
pcfg <- update(pcfg)

#Write CDO grid descriptor
writeLines(griddes(pcfg),pcfg@analysis.grid)

#Output
save(pcfg,file="objects/configuration.RData")
save(pcfg,file=file.path(pcfg@scratch.dir,"configuration.RData"))

# ========================================================================
# Done
# ========================================================================
#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nConfiguration complete.\n")

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
