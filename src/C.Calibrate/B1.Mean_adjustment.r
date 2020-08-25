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
frag.tbl <- tbl(this.db,PE.cfg$db$extract)
clim.tbl <- tbl(this.db,PE.cfg$db$climatology)


#Clear all previous analyses
del.this <-
  tbl(this.db,PE.cfg$db$calibration) %>%
  filter(calibrationMethod %in% c("anomaly","Mean adjusted")) %>%
  select(pKey) %>%
  collect() %>%
  pull(pKey) 
PE.db.delete.by.pKey(pcfg,tbl.name=PE.cfg$db$calibration,del.this)

#Import observational climatology data
obs.clim.dat <-
  clim.tbl %>%
  filter(srcType=="Observations") %>%
  collect() %>% 
  PE.db.unserialize() %>% 
  select(month,data) %>%
  rename(data.obsClim=data)

#Import model climatology data
mdl.clim.dat <-
  clim.tbl %>%
  filter(srcType!="Observations") %>%
  collect() %>% 
  PE.db.unserialize() %>% 
  select(-nYears) %>%
  rename(data.mdlClim=data)

#Import model data 
mdl.dat <-
  frag.tbl %>%
  filter(srcType != "Observations") %>%
  collect() %>%
  PE.db.unserialize() %>%
  #Calculate date information
  mutate(date=ymd(date),
         month=month(date)) %>%
  rename(data.mdl=data)

#Finished import
dbDisconnect(this.db)

#'========================================================================
# Recalibration ####
#'========================================================================
log_msg("Merging...\n")
all.dat <- 
  mdl.dat %>%
  #Join in model climatological data
  left_join(y=mdl.clim.dat,
            by=c("srcName","srcType","month","leadIdx")) %>%
  #Join in observational climatology 
  left_join(y=obs.clim.dat,
            by=c("month"))

log_msg("Recalibration...\n")
calib.dat <-
  all.dat %>%
  #Calculate the anomaly and make the correction
  mutate(data.anom=map2(data.mdl,data.mdlClim,~ .x - .y),
         data.calib=map2(data.anom,data.obsClim,~ .x + .y))

#'========================================================================
# Output ####
#'========================================================================
log_msg("Output...\n")
out.dat <- 
  calib.dat %>%
  #Select columns and tidy
  select(srcName,srcType,realization,startDate,date,leadIdx,data.anom,data.calib) %>% 
  mutate(date=as.character(date)) %>%
  rename("anomaly"=data.anom,"Mean adjusted"=data.calib) %>%
  #Pivot
  pivot_longer(c(anomaly,`Mean adjusted`),
               names_to = "calibrationMethod",
               values_to = "data")

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
