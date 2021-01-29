#'========================================================================
# B3. Mackerel Summer Feeding
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Jul 26 14:50:42 2018
#
# Defines the configuration parameters for assessing the predictability of
# Mackerel summer feeding conditions and the possible predictability of the
# the Mackerel war

# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","B9. Mackerel Summer Feeding"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
})
load(PE.cfg$path$datasrcs)

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "Mackerel_summer",
                       MOI=8,
                       average.months=FALSE,
                       clim.years=1981:2010,  
                       comp.years=1970:2015,
                       landmask="data_srcs/NMME/landmask.nc",
                       Observations=SST_obs[[c("HadISST")]],#,
                       calibrationMethods=c("anomaly","MeanAdj","MeanVarAdj"),
                       NMME=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path("scratch",pcfg@project.name)
define_dir(pcfg@scratch.dir)

#Setup persistence
pcfg@persistence.leads <- seq(pcfg@MOI-1,120,by=12)

#Select decadal models
pcfg@Decadal <- SST.Decadal.production

#Select CMIP5 models
#pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="tos")

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-70,0,50,80)
pcfg@global.res  <- 0.5
pcfg@retain.realizations <- TRUE

#Import EEZ's
load("resources/EEZs/EEZs.RData")

#Split Greenlandic EEZ into an east and west EEZ, and carve off the top
N.lim <- 70
greenland.EW.split <- -45
eez.gland.full <- 
  filter(eez.sf,str_detect(GeoName,c("Greenland"))) %>%
  st_crop(xmin=-180,xmax=180,ymin=0,ymax=N.lim) %>%
  mutate(name="Greenland")
eez.gland.W <- 
  st_crop(eez.gland.full,xmin=-180,xmax=greenland.EW.split,ymin=0,ymax=N.lim) %>%
  mutate(name="West_Greenland")
eez.gland.E <- 
  st_crop(eez.gland.full,xmin=greenland.EW.split,xmax=180,ymin=0,ymax=N.lim) %>%
  mutate(name="East_Greenland")

#Extract Iceland
eez.iceland <- 
  filter(eez.sf,str_detect(GeoName,c("Iceland")),Pol_type=="200NM") %>%
  mutate(name="Iceland")

#Add a regional domain as well
sp.regional <- 
  st_sf(name="regional",geometry=st_sfc(sfpolygon.from.extent(extent(-60,0,56,70))),
        crs=crs(eez.iceland))

#Add to object
pcfg@spatial.polygons <- 
  rbind(eez.iceland, eez.gland.E,eez.gland.W,eez.gland.full) %>%
  select(name,geometry) %>%
  rbind(sp.regional)

#'========================================================================
# Summary statistics ####
#'========================================================================
#Configure summary stats
statsum.l <- PElst()
statsum.l$Jansen <- threshold(name="JansenTreshold",
                            desc = "Area above 8.5 degrees",
                            above=TRUE,
                            calibration = c("MeanAdj","MeanVarAdj"),
                            retain.field = FALSE,
                            realizations=1:4,
                            threshold=8.5)  #Based on Jansen et al
# statsum.l$temp <- spatial.mean(name="TempAnomaly",
#                                desc="Temperature anomaly",
#                                calibration="anomaly")

#Setup habitat suitability functionality
habitat.mdl.dat <- readRDS("resources/Mackerel_summer_QR_values.rds")
resource.l <-  list(fun=approxfun(habitat.mdl.dat$temp,habitat.mdl.dat$value,rule=2))
habitat.fn <- function(dat,resources) {
  #Evaluate habitat suitability function
  hab.suit <- dat
  hab.suit[] <- exp(resources$fun(dat[]))
  
  # Field values --------------------------------------------------------------------
  field.l <- list()  #Results storage
  field.l$habitatSuitability <- hab.suit

  # Scalar values -------------------------------------------------------------------
  scalar.l <- vector()
  pxl.area <- area(hab.suit)
  scalar.l["carryingCapacity"] <-
    cellStats(pxl.area* hab.suit,sum,na.rm=TRUE)

  #Return results
  this.rtn <- 
    bind_rows(enframe(field.l,"resultName","field"),
              enframe(scalar.l,"resultName","value"))

  return(this.rtn)
}

statsum.l$HabitatModel <-  custom.stat(name="HabitatModel",
                           desc="Quantile regression habitat model",
                           fn=habitat.fn,
                           resources=resource.l,
                           calibration = c("MeanAdj","MeanVarAdj"),
                           retain.field=FALSE,
                           realizations=1:4)

#Merge it all in
pcfg@statistics <- statsum.l

#'========================================================================
# Done
#'========================================================================
#Output
set.configuration(pcfg)

#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nConfiguration complete.\n")

# .............
# This work by Mark R Payne is licensed under a  Creative Commons
# Attribution-NonCommercial-ShareAlike 3.0 Unported License.
# For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
# Basically, this means that you are free to "share" and "remix" for
# non-commerical purposes as you see fit, so long as you "attribute" me for my
# contribution. Derivatives can be distributed under the same or
# similar license.
#
# This work comes with ABSOLUTELY NO WARRANTY or support.
#
# This work should also be considered as BEER-WARE. For details, see
# http://en.wikipedia.org/wiki/Beerware
# .............
