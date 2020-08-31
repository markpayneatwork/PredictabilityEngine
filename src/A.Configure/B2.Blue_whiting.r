#/*##########################################################################*/
#' Blue whiting Configuration
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' TSun Sep  4 23:22:56 2016
#'
#' Configures a blue whiting  object
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
cat(sprintf("\n%s\n","Blue Whiting Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
})
load(PE.cfg$path$datasrcs)

# ========================================================================
# Generic Configuration
# ========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "Blue_whiting",
                       MOI=3,  #March - ideally should be Feb-March-april average (??)
                       average.months=FALSE,
                       clim.years=1982:2005,  
                       comp.years=1970:2015,
                       Observations = Sal.obs$EN4,
                       landmask="data_srcs/NMME/landmask.nc")

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)
define_dir(pcfg@scratch.dir)

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-25,0,40,65)
pcfg@global.res  <- 0.5
pcfg@vert.range <- c(250,600)

#Polygons
sp.objs <- list()
sp.objs$"Spawning area" <- sfpolygon.from.extent(extent(-20,-5,50,60))
pcfg@spatial.polygons <- 
  sp.objs %>% enframe(value="geometry") %>% st_sf()

#'========================================================================
# Data Sources ####
#'========================================================================
#Define observational sources
#pcfg@Observations <- Sal.obs$EN4

#Decadal salinity models
pcfg@Decadal <- Sal.Decadal

#CMIP5 salinity
#pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="so")

#'========================================================================
# Statistics ####
#'========================================================================
#Configure stats
stat.l <- PElst()

#Average salinity
stat.l[["MeanSal"]]  <- spatial.mean(name="Mean-salinity",
                                     desc="Mean salinity",
                                     realizations=c(1:3),
                                     calibration="Mean adjusted")

#Full salinity field and anomaly
stat.l[["SalAnomField"]] <- pass.through(name="SalAnomaly",
                                         desc="Salinity anomaly",
                                         realizations=1:3,
                                         calibration="anomaly")

#Setup Miesner & Payne habitat model
require(mgcv)
#Setup bathymetric grid 
#Note that we need to account for the extraction buffer here, to get a 
#perfect match with the rest of the data
log10bath <- raster("resources/BlueWhiting/ETOPO1_Bed_c_gmt4.grd") %>%
              crop(extent(pcfg@global.ROI)) %>%
              raster::aggregate(fact=pcfg@global.res/res(.),fun=mean)
log10bath <- log10(-log10bath)

#if(max(abs(extent(log10bath)[1:4]-pcfg@global.ROI[1:4]))>1e-9) stop("Mismatch in bathymetric dimensions")
if(!identical(res(log10bath)[1],pcfg@global.res)) stop("Mismatch in bathymetric resolution")

#Setup latitude raster
lat.rast <- log10bath
lat.rast[] <- yFromCell(lat.rast,1:ncell(log10bath))

#Setup prediction grid
pred.l <- list(latitude=lat.rast,
                       log10bath=log10bath)
pred.consts <- data.frame(doy=seq(30,180,by=3),
                          sol.el=0)

#Setup resource list
GAM.sdm.resources <- list(model=readRDS("resources/BlueWhiting/BW_GAM_SDM.rds"),
                          apply.threshold=TRUE,
                          pred.l=pred.l,
                          pred.consts=pred.consts)

#Setup prediction function
GAM.sdm.fn <- function(dat,resources) {
  require(mgcv)
  #Crop the prediction data down to the same scale as the values
  res.l <- vector("list",nlayers(dat))
  for(l in seq(nlayers(dat))) {
    this.layer <- dat[[l]]
    pred.dat <- brick(c(resources$pred.l,EN4.salinity=this.layer))
    #Loop over rows in prediction constants
    p.l <- vector("list",nrow(resources$pred.consts))
    for(i in seq(nrow(resources$pred.consts))) {
      p.l[[i]] <- raster::predict(object=pred.dat,
                           model=resources$model,
                           fun=predict.gam,
                           const=resources$pred.consts[i,], 
                           type="response")
      
    }
    #Compress into a brick
    p <- brick(p.l)
    if(resources$apply.threshold) {
      res.l[[l]] <- sum(p > resources$model$threshold)
    } else {
      stop("Dunno what's going on here")
      res.l[[l]] <- p }
  }
  rtn <- brick(res.l)
  return(rtn)
}

#Setup to look across all days of year
stat.l[["SDM_range"]] <- habitat(name="SDMrange",
                           desc="Habitat is suitable on at least one day",
                           fn=GAM.sdm.fn,
                           resources=GAM.sdm.resources,
                           skill.metrics = "correlation",
                           calibration="Mean adjusted",
                           realizations = 1:3)

#Just focus on 15th April (DOY=105) => Spawning in mid March
GAM.sdm.resources$pred.consts <- data.frame(doy=105,
                                            sol.el=0)
stat.l[["SDM15apr"]] <- habitat(name="SDM15Apr",
                               desc="Habitat is suitable on 15 Apr",
                               fn=GAM.sdm.fn,
                               resources=GAM.sdm.resources,
                               skill.metrics = "correlation",
                               calibration="Mean adjusted",
                               realizations=1:3)

GAM.sdm.resources$apply.threshold <- FALSE
stat.l[["SDM15apr_nothreshold"]] <- habitat(name="SDM15AprNoThresh",
                                desc="Habitat is suitable on 15 Apr, without a threshold being applied",
                                fn=GAM.sdm.fn,
                                resources=GAM.sdm.resources,
                                skill.metrics = "correlation",
                                calibration="Mean adjusted",
                                realizations=1:3)

# stat.l[["SDM-PA-reals"]] <- habitat(name="SDM-PA-reals",
#                        desc="SDM based on individual realisations, PA",
#                        fn=GAM.sdm.fn,
#                        resources=GAM.sdm.resources,
#                        skill.metrics = "correlation",
#                        use.full.field = TRUE,
#                        use.realmeans = FALSE)

#Add a stat that returns the probability as well
# GAM.sdm.resources$apply.threshold <- FALSE
# 
# stat.l[["SDM-Prob-realmean"]] <- habitat(name="SDM-prob-realmean",
#                                      desc="SDM based on realisation means predicting probability",
#                                      fn=GAM.sdm.fn,
#                                      resources=GAM.sdm.resources,
#                                      skill.metrics = "correlation",
#                                      use.full.field = TRUE,
#                                      use.realmeans=TRUE)

#Merge it all in
pcfg@statistics <- stat.l


#'========================================================================
# Output ####
#'========================================================================
set.configuration(pcfg)

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
