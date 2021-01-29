#'========================================================================
# A1.Climatological statistics
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Aug 14 16:37:26 2020
#
# Calculates climatological statistics (mainly the mean, but also potentially
# variance etc) over the extracted database. These statistics are used as the
# basic for subsequent recalibration approaches.
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
cat(sprintf("\n%s\n","A1.Climatological statistics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressMessages({
  library(PredEng)
  library(ncdf4)
  library(lubridate)
})
pcfg <- PE.load.config()
  
#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
set.log_msg.silent()

#'========================================================================
# Setup ####
#'========================================================================
#Setup databases
this.db <- PE.db.connection(pcfg,PE.cfg$db$extract)
extr.tbl <- tbl(this.db,PE.cfg$db$extract)

#'========================================================================
# Calculate climatologies ####
#'========================================================================
#Get list of frags that are in the climatological years
# Note that we do this across the realisation means. This is deliberate, as 
# it means that we can handle situations where the number of realizations
# changes over time e.g. as in NMME.
# Note that we avoid loading everything into memory at once
extr.these.pKeys<-
  extr.tbl %>%
  filter(realization == "realmean" | srcType == "Observations") %>%
  select(pKey,date,srcType,srcName) %>%
  collect() %>%
  mutate(date=ymd(date),
         year=year(date)) %>%
  filter(year %in% pcfg@clim.years) %>%
  pull(pKey)

extr.dat <-
  extr.tbl %>%
  filter(pKey %in% extr.these.pKeys) %>%
  select(-srcFname) %>%
  collect() %>%
  mutate(date=ymd(date),
         month=month(date)) %>%
  PE.db.unserialize() 
dbDisconnect(this.db)

#Calculate climatologies in a summarisation loop
clim.dat <-
  extr.dat %>%
  group_by(srcType,srcName,leadIdx,month,.drop=TRUE) %>%
  summarise(clim.mean=raster.list.mean(field),
            clim.sd=list(calc(brick(field),sd)),
            nYears=n(),
            .groups="keep")

#Pivot longer 
clim.out <- 
  clim.dat %>%
  ungroup() %>%
  pivot_longer(starts_with("clim."),
               names_to="statistic",
               values_to="field") %>%
  #Make sure everything is in memory
  mutate(statistic=gsub("clim.","",statistic),
         field=map_if(field,~!inMemory(.x),~readAll(.x)))

#Write results
#Reset the resutls table by deleting the file and reestablishing it
file.remove(PE.db.path(pcfg,PE.cfg$db$climatology))
PE.db.setup(pcfg)
PE.db.appendTable(pcfg,PE.cfg$db$climatology,clim.out)

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(length(warnings())!=0) print(warnings())
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
