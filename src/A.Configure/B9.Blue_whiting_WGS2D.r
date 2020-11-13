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
pcfg <- PredEng.config(project.name= "Blue_whiting_WGS2D",
                       MOI=3,  #March - ideally should be Feb-March-april average (??)
                       obs.only=TRUE,
                       average.months=FALSE,
                       clim.years=1981:2009,  
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
pcfg@global.res  <- 0.25
pcfg@vert.range <- c(250,600)

#Polygons
sp.objs <- list()
#sp.objs$"Spawning area" <- sfpolygon.from.extent(extent(-20,-5,50,60))
sp.objs$"Spawning area" <- sfpolygon.from.extent(pcfg@global.ROI)
pcfg@spatial.polygons <- 
  sp.objs %>% enframe(value="geometry") %>% st_sf()

#'========================================================================
# Data Sources ####
#'========================================================================
#Define observational sources
pcfg@Observations <- Sal.obs$EN4

#'========================================================================
# Statistics ####
#'========================================================================
#Configure stats
stat.l <- PElst()

#Average salinity
stat.l[["MeanSal"]]  <- spatial.mean(name="Mean-salinity",
                                     desc="Mean salinity",
                                     realizations = 1,
                                     calibration="Mean adjusted")

#Setup Miesner & Payne habitat model
require(mgcv)
#Setup bathymetric grid 
#Note that we need to account for the extraction buffer here, to get a 
#perfect match with the rest of the data
log10bath <- raster("resources/BlueWhiting/ETOPO1_Bed_c_gmt4.grd") %>%
  crop(extent(pcfg@global.ROI)) %>%
  raster::aggregate(fact=pcfg@global.res/res(.),fun=mean)
log10bath <- log10(-log10bath)
if(!identical(res(log10bath)[1],pcfg@global.res)) stop("Mismatch in bathymetric resolution")
lat.rast <- log10bath    #Setup latitude raster
lat.rast[] <- yFromCell(lat.rast,1:ncell(log10bath))

#Setup resource list
GAM.sdm.resources <- 
  list(model=readRDS("resources/BlueWhiting/BW_GAM_SDM.rds"),
       pred.l=list(latitude=lat.rast,
                   log10bath=log10bath))

#Setup prediction function
GAM.sdm.fn <- function(dat,resources) {
  require(mgcv)
  #Setup
  grid.dt <- 3
  pred.consts <-
    tibble(doy=seq(30,180,by=grid.dt),
           sol.el=0)
  assert_that(nlayers(dat)==1,msg="Inputs with multiple layers not supported")
  pred.dat <- brick(c(resources$pred.l,EN4.salinity=dat))
  
  #Loop over rows in prediction constants
  p.l <- vector("list",nrow(pred.consts))
  for(i in seq(nrow(pred.consts))) {
    p.l[[i]] <- raster::predict(object=pred.dat,
                                model=resources$model,
                                fun=predict.gam,
                                const=pred.consts[i,], 
                                type="response")
  }
  pred.b <- brick(p.l)#Compress into a brick
  
  # Field values --------------------------------------------------------------------
  field.l <- list()  #Results storage
  #Maximum probability
  field.l$maximumProbability <- max(pred.b)
  #Suitable habitat at some point
  field.l$suitableHabitat <- field.l$maximumProbability > resources$model$threshold
  #Days of suitable habitat
  field.l$daysSuitableHabitat <- sum(pred.b > resources$model$threshold) * grid.dt
  #15 April
  field.l$april15 <- pred.b[[which(pred.consts$doy==105)]]
  
  # Scalar values -------------------------------------------------------------------
  scalar.l <- vector()
  pxl.area <- area(pred.b)
  scalar.l["areaSuitableHabitat"] <- cellStats(pxl.area* field.l$suitableHabitat,sum,na.rm=TRUE)
  scalar.l["area15April"] <- cellStats(pxl.area* field.l$april15,sum,na.rm=TRUE)
  #Westward extent 
  west.ext <- function(r) {
    west.focus <- crop(field.l$suitableHabitat,extent(-25,0,54,58))
    west.ext.df <- 
      rasterToPoints(west.focus) %>%
      as_tibble() %>%
      filter(layer==1) %>%
      group_by(y) %>%
      summarise(min.x=min(x)) 
    return(mean(west.ext.df$min.x,na.rm=TRUE))
  }
  scalar.l["westwardExtentMax"] <- west.ext(field.l$suitableHabitat)
  scalar.l["westwardExtent15April"] <- west.ext(field.l$april15)

  #Return results
  this.rtn <- 
    bind_rows(enframe(field.l,"resultName","field"),
              enframe(scalar.l,"resultName","value"))
  
  return(this.rtn)
}

#Setup to look across all days of year
stat.l$SDM <- 
  custom.stat(name="SDM",
              desc="Apply Miesner and Payne 2018 habitat model",
              fn=GAM.sdm.fn,
              resources=GAM.sdm.resources,
              skill.metrics = "correlation",
              realizations=1,
              calibration="Mean adjusted")

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
