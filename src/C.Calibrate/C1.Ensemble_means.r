#'========================================================================
# C1.Ensemble_means
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Aug 20 08:54:49 2020
#
# Calculate the ensemble means of the calibrated fragments
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
cat(sprintf("\n%s\n","C1.Ensemble_means"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressPackageStartupMessages({
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
# Setup ####
#'========================================================================
log_msg("Import data..\n")
#Setup databases
this.db <- PE.db.connection(pcfg)
calib.tbl <- tbl(this.db,PE.cfg$db$calibration)

#Get list of realisation means
realmeans <-
  calib.tbl %>%
  filter(realization=="realmean") %>%
  collect() %>%
  PE.db.unserialize()

#Clear all previous ensemble means
prev.ensmeans <- 
  calib.tbl %>%
  filter(srcName=="ensmean") %>%
  select(pKey) %>%
  collect() %>%
  pull() 
dbDisconnect(this.db)  #Finished with database
PE.db.delete.by.pKey(pcfg,tbl.name=PE.cfg$db$calibration,pKeys = prev.ensmeans,silent=FALSE)

#'========================================================================
# Process ####
#'========================================================================
#Assertion: all months are rounded down to the first of the month, and therefore
#the date field is always 01. 
assert.day <- 
  realmeans %>%
  mutate(date=ymd(date),
         day=day(date)) 
assert_that(all(assert.day$day==1),msg="Day = 1 condition violated")

#Throw it all at dplyr
if(!pcfg@obs.only) {
  ensmeans <- 
    realmeans %>%
    group_by(srcType,calibrationMethod,startDate,date,leadIdx,.drop=TRUE) %>%
    summarise(field=raster.list.mean(field),
              n=n(),
              .groups="keep") %>% #Check for duplicated realization codes
    ungroup() %>%
    add_column(srcName="ensmean",.after=1)
  
  #Write results
  ensmeans %>%
    select(-n) %>%
    mutate(realization="ensmean") %>%
    PE.db.appendTable(pcfg,tbl.name = PE.cfg$db$calibration)
}


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
