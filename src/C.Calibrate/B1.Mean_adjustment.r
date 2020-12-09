#'========================================================================
# B1. Mean adjustment
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Aug 14 16:37:26 2020
#
# Performs a recalibration of all model data based on a simple mean adjustment
#
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
cat(sprintf("\n%s\n","B1. Mean adjustment"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressMessages({
  library(PredEng)
  library(ncdf4)
  library(lubridate)
})
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.log_msg.silent()
} else {
  #Do everything and tell us all about it
  set.log_msg.silent(FALSE)
}

#'========================================================================
# Import data ####
#'========================================================================
log_msg("Import data..\n")
#Setup databases
this.db <- PE.db.connection(pcfg)
extr.tbl <- tbl(this.db,PE.cfg$db$extract)
clim.tbl <- tbl(this.db,PE.cfg$db$climatology)

#Clear all previous analyses that give these types of calibration methods
del.this <-
  tbl(this.db,PE.cfg$db$calibration) %>%
  filter(calibrationMethod %in% c("anomaly","MeanAdj","MeanVarAdj")) %>%
  select(pKey) %>%
  collect() %>%
  pull(pKey) 
PE.db.delete.by.pKey(pcfg,tbl.name=PE.cfg$db$calibration,del.this)

#Import observational climatology data for the month in question
#Note that the extraction process has already ensured for all of the models
#that the only thing in the extraction is the month of interest. However, for
#the observations, we have all months present. However, in this case we want to
#adjust the anomaly to the climatological observed value in the MOI. We therefore only
#need one value here.
#TODO: When implementing multiple MOIs, would extract relevant MOIs and average, I guess
obs.clim <-
  clim.tbl %>%
  filter(srcType=="Observations",
         month %in% !!pcfg@MOI) %>%
  collect() %>% 
  PE.db.unserialize() %>%
  pivot_wider(names_from=statistic,values_from=field) %>%
  select(mean,sd)
dmp <- assert_that(nrow(obs.clim)==1,msg = "Multiple rows detected in observational climatology.")

#Import climatology data from both observations and model forecasts
clim.dat <-
  clim.tbl %>%
  collect() %>% 
  PE.db.unserialize() %>% 
  pivot_wider(names_from=statistic,values_from=field)%>%
  rename(mdlClim.mean=mean,mdlClim.sd=sd)

#TODO:Work through extraction table systematically based on the pKeys that are present
extr.dat <-
  extr.tbl %>%
  collect() %>%
  PE.db.unserialize() %>%
  #Calculate date information
  mutate(date=ymd(date),
         month=month(date)) %>%
  rename(field.extr=field)

#Finished import
dbDisconnect(this.db)

#'========================================================================
# Recalibration ####
#'========================================================================
log_msg("Merging...\n")
all.dat <- 
  extr.dat %>%
  #Join in model climatological data
  left_join(y=clim.dat,
            by=c("srcName","srcType","month","leadIdx")) 

log_msg("Recalibration...\n")
calib.dat <-
  all.dat %>%
  #Calculate the anomaly and make the correction
  mutate(field.anom=map2(field.extr,mdlClim.mean,~ .x - .y),
         field.meanAdjust=map(field.anom,~ .x + obs.clim$mean[[1]]),
         field.meanvarAdjust=map2(field.anom,mdlClim.sd,~(.x/.y)*obs.clim$sd[[1]]+obs.clim$mean[[1]]))

#'========================================================================
# Output ####
#'========================================================================
log_msg("Output...\n")
out.dat <- 
  calib.dat %>%
  #Select columns and tidy
  select(srcName,srcType,realization,startDate,date,leadIdx,
         field.anom,field.meanAdjust,field.meanvarAdjust) %>% 
  mutate(date=as.character(date)) %>%
  rename("anomaly"=field.anom,
         "MeanAdj"=field.meanAdjust,
         "MeanVarAdj"=field.meanvarAdjust) %>%
  #Pivot
  pivot_longer(-c(srcName,srcType,realization,startDate,date,leadIdx),
               names_to = "calibrationMethod",
               values_to = "field") %>%
  #Drop unsupported calibrationMethods
  filter(calibrationMethod %in% pcfg@calibrationMethods)
  

#Write results
PE.db.appendTable(out.dat,pcfg,PE.cfg$db$calibration)

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

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
