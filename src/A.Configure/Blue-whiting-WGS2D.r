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
these.srcs <- readRDS(PE.cfg$path$datasrcs)

# ========================================================================
# Generic Configuration
# ========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "Blue-whiting-WGS2D",
                       MOI=3,  #March - ideally should be Feb-March-april average (??)
                       obs.only=TRUE,
                       average.months=FALSE,
                       clim.years=1981:2010,  
                       comp.years=1970:2015,
                       calibrationMethods=c("MeanAdj"),
                       landmask="data_srcs/NMME/landmask.nc")

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)
define_dir(pcfg@scratch.dir)

#'========================================================================
# Data sources ####
#'========================================================================
#' Observations
pcfg@Observations <- 
  filter(these.srcs,
         group=="Sal.obs",srcName=="EN4") %>%
  pull(sources) %>%
  pluck(1)

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-25,0,40,65)
pcfg@global.res  <- 0.25
pcfg@vert.range <- c(250,600)

#Polygons
sp.objs <- list()
#sp.objs$"WestExtent" <- sfpolygon.from.extent(extent(-25,0,56,58))
sp.objs$"NorthernComponent" <- sfpolygon.from.extent(extent(-25,-5,54,58))

pcfg@spatial.polygons <- 
  sp.objs %>% enframe(value="geometry") %>% st_sf()

#'========================================================================
# Pointwise extraction ####
#'========================================================================
#Extract the data used in creating the Blue Whiting SDM from the EN4 product.
#Doing the comparison in this way lets us check whether it has worked
CPR.pts <- 
  readRDS(here("resources/BlueWhiting/model_dat.rds")) %>%
  select(haulID,latitude,longitude,date=datetime,EN4.salinity) %>%
  mutate(date=as.Date(date)-months(1)) %>%  #Account for spawning one month before
  st_as_sf(coords=c("longitude","latitude")) %>%
  list() %>%
  tibble(extrName="CPR",
         table=PE.cfg$db$extract,
         filter='srcType=="Observations" & srcName=="EN4"',
         points=.)

IBWSS.SA.pts <-
  readRDS(here("resources/BlueWhiting/IBWSS_BW_SA.rds")) %>%
  mutate(type="SA",
         date=as.Date(ISOdate(YEAR,3,15))) %>%
  list() %>%
  tibble(extrName="IBWSS_SA",
         table=PE.cfg$db$stat,
         filter='statName =="SDM" & srcType=="Observations" & spName=="global.ROI"',
         points=.)

IBWSS.CTD.pts <-
  readRDS(here("resources/BlueWhiting/IBWSS_CTD.rds")) %>%
  mutate(type="CTD",
         date=as.Date(ISOdate(YEAR,3,15))) %>%
  list() %>%
  tibble(extrName="IBWSS_CTD",
         table=PE.cfg$db$extract,
         filter='srcType=="Observations" & srcName=="EN4"',
         points=.)

NEAFC14.pts <-
  readRDS(here("resources/BlueWhiting/NEAFC14_data.rds")) %>%
  mutate(date=as.Date(ISOdate(year,3,15)))%>%
  list()%>%
  tibble(extrName="NEAFC14",
         table="statistics",
         filter='statName =="SDM" & srcType=="Observations" & spName=="global.ROI"',
         points=.)

pcfg@pt.extraction <- 
  bind_rows(CPR.pts,
            IBWSS.SA.pts,
            IBWSS.CTD.pts,
            NEAFC14.pts)

#'========================================================================
# Statistics ####
#'========================================================================
#Configure stats
stat.l <- PElst()

#Average salinity
stat.l[["MeanSal"]]  <- spatial.mean(name="Mean-salinity",
                                     desc="Mean salinity")

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

# Threshold values are extracted by matching prevalence in IBWSS survey across the
# main spawning region. See ~/gbar/Blue_whiting/IBWSS project for more details
# The threshold for the blue whiting larvae from the SDM is stored in model$threshold
GAM.sdm.resources$thresholds <- 
  list(april15=0.154,
       maximumProbability=0.208,
       meanProbability=0.113,
       matchSurvey=0.25,
       larvae=GAM.sdm.resources$model$threshold)
GAM.sdm.resources$doys <-seq(30,180,by=3)
GAM.sdm.resources$WGS2D <- TRUE

#Setup prediction function
GAM.sdm.fn <- function(dat,resources,retain) {
  #Debug
  # dat <- masked.dat
  # resources <- this.stat@resources
  
  require(mgcv)
  #Setup
  if(resources$WGS2D) {
    pred.consts <-
      tibble(doy=105,
             sol.el=0)
  } else {
    pred.consts <-
      tibble(doy=resources$doys,
             sol.el=0)
  }
  assert_that(nlayers(dat)==1,msg="Inputs with multiple layers not supported")
  pred.l <- lapply(resources$pred.l,crop,dat)
  pred.dat <- brick(c(pred.l,EN4.salinity=dat))
  
  #Loop over rows in prediction constants
  p.l <- vector("list",nrow(pred.consts))
  for(i in seq(nrow(pred.consts))) {
    p.l[[i]] <- raster::predict(object=pred.dat,
                                model=resources$model,
                                fun=predict.gam,
                                const=pred.consts[i,], 
                                type="response")
  }
  pred.b <- brick(p.l) #Compress into a brick

  #Results storage  
  field.l <- list()  #Results storage
  scalar.l <- vector()
  pxl.area <- area(pred.b)
  
  # Field values --------------------------------------------------------------------
  if(resources$WGS2D) {
    #Maximum probability
    field.l$maximumProbability <- max(pred.b)
    #Suitable habitat at some point
    field.l$meanProbability <- mean(pred.b)
  }
  #15 April
  #We use 15th april as the larvae habitat value and in decadal runs
  field.l$april15 <- pred.b[[which(pred.consts$doy==105)]]
  
  # Scalar values -------------------------------------------------------------------
  if(resources$WGS2D) {
    scalar.l["areaMaxProbability"] <-
      cellStats(pxl.area* (field.l$maximumProbability > resources$thresholds$maximumProbability),
                sum,na.rm=TRUE)
    scalar.l["areaMeanProbability"] <-
      cellStats(pxl.area* (field.l$meanProbability >resources$thresholds$meanProbability),
                sum,na.rm=TRUE)
    scalar.l["area15April"] <- 
      cellStats(pxl.area* (field.l$april15 > resources$thresholds$april15),
                sum,na.rm=TRUE)
  }
  scalar.l["areaLarvae"] <- 
    cellStats(pxl.area* (field.l$april15 > resources$thresholds$larvae),
              sum,na.rm=TRUE)
  scalar.l["areaMatchSurvey"] <- 
    cellStats(pxl.area* (field.l$april15 > resources$thresholds$matchSurvey),
              sum,na.rm=TRUE)
  #Westward extent 
  west.ext <- function(r,this.threshold) {
    west.focus <- crop(r,extent(-25,0,56,58))
    west.ext.df <- 
      (west.focus > this.threshold ) %>%
      rasterToPoints() %>%
      as_tibble() %>%
      filter(layer==1) %>%
      group_by(y) %>%
      summarise(min.x=min(x),
                .groups="drop") 
    return(mean(west.ext.df$min.x,na.rm=TRUE))
  }
  if(resources$WGS2D) {
    scalar.l["westwardExtentMaxProb"] <- 
      west.ext(field.l$maximumProbability,resources$thresholds$maximumProbability)
    scalar.l["westwardExtentMeanProb"] <- 
      west.ext(field.l$meanProbability,resources$thresholds$meanProbability)
    scalar.l["westwardExtent15April"] <- 
      west.ext(field.l$april15,resources$thresholds$april15)
  }
  scalar.l["westwardExtentLarvae"] <- 
    west.ext(field.l$april15,resources$thresholds$larvae)
  scalar.l["westwardExtentMatchSurvey"] <- 
    west.ext(field.l$april15,resources$thresholds$matchSurvey)
  
  #Calculate westward extent using clumping approach
  west.ext.clump <- function(r,this.threshold) {
    west.focus <- crop(r,extent(-25,0,56,58))
    west.ext.clump <- 
      (west.focus > this.threshold ) %>%
      clump()
    west.ext.df <<-
      west.ext.clump %>%
      rasterToPoints() %>%
      as_tibble() %>%
      group_by(clumps,y) %>%
      summarise(min.x=min(x),
                .groups="drop") %>%
      group_by(clumps) %>%
      summarise(mean.min.x=mean(min.x),
                .groups="drop") 
    return(max(west.ext.df$mean.min.x))
  }
  scalar.l["westExtClumpLarvae"] <- 
    west.ext.clump(field.l$april15,resources$thresholds$larvae)
  scalar.l["westExtClumpMatchSurvey"] <- 
    west.ext.clump(field.l$april15,resources$thresholds$matchSurvey)
  
  #Return results
  if(retain) {
    this.rtn <- 
      bind_rows(enframe(field.l,"resultName","field"),
                enframe(scalar.l,"resultName","value"))
  } else {
    this.rtn <- enframe(scalar.l,"resultName","value")
  }

  return(this.rtn)
}

#Setup to look across all days of year
stat.l$SDM <- 
  custom.stat(name="SDM",
              desc="Miesner and Payne 2018 habitat model applied to the GlobalROI",
              fn=GAM.sdm.fn,
              resources=GAM.sdm.resources,
              sources = 0:1,
              use.globalROI = TRUE,
              retain.field=TRUE)

#Merge it all in
pcfg@statistics <- stat.l

#'========================================================================
# Output ####
#'========================================================================
pcfg <- set.configuration(pcfg)

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
