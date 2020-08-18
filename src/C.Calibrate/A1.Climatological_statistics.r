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
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

#Helper functions, externals and libraries
library(PredEng)
library(ncdf4)
library(lubridate)
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
#Setup databases
this.db <- PE.db.connection(pcfg)
frag.tbl <- tbl(this.db,PE.cfg$db$extract)

#Reset the resutls table by deleting and reestablishing it
dbRemoveTable(this.db,PE.cfg$db$climatology)
PE.db.setup(pcfg)

#'========================================================================
# Calculate climatologies ####
#'========================================================================
#Get list of frags that are in the climatological years
# Note that we do this across the realisation means. This is deliberate, as 
# it means that we can handle situations where the number of realizations
# changes over time e.g. as in NMME.
pcfg@clim.years <- 1960:2005
clim.frag.dat <-
  frag.tbl %>%
  filter(realization == "realmean" | srcType =="Observations") %>%
  collect() %>%
  mutate(date=ymd(date),
         year=year(date),
         month=month(date))%>%
  filter(year %in% pcfg@clim.years) %>%
  PE.db.unserialize()

#Prepare to loop!
clim.frag.dat <-
  group_by(clim.frag.dat,srcName,srcType,leadIdx,month,.drop=TRUE)

#Calculate climatologies in a summarisation loop
clim.dat <-
  clim.frag.dat %>%
  summarise(data=raster.list.mean(data),
            nYears=n())

#Write results
PE.db.appendTable(clim.dat,this.db,PE.cfg$db$climatology)

#Finished with output
dbDisconnect(this.db)

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

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
