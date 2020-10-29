#'========================================================================
# Extract_results_from_SQlite
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Oct 29 12:04:17 2020
#
# Extracts the results tables from SQLite and writes them into a separate
# database
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
cat(sprintf("\n%s\n","Extract_results_from_SQlite"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)
library(PredEng)
pcfg <- readRDS(here(PE.cfg$path$config))

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
include.field.stats <- FALSE

#'========================================================================
# Setup ####
#'========================================================================
#Open existing
src.db <- PE.db.connection(pcfg)
mets.tbl <- tbl(src.db,PE.cfg$db$metrics)

#Create new results db
res.db.fname <- here(pcfg@scratch.dir,sprintf("%s_results.sqlite",pcfg@project.name))
if(file.exists(res.db.fname)) {
  unlink(res.db.fname,force=TRUE)
}
PE.db.setup(pcfg,results.only = TRUE)
res.db <- PE.db.connection(pcfg,results.db = TRUE)

#'========================================================================
# Paste across to new DB ####
#'========================================================================
#First the stats
if(include.field.stats) {
  stats.tbl <- 
    tbl(src.db,PE.cfg$db$stats)
} else {
  stats.tbl <- 
    tbl(src.db,PE.cfg$db$stats) %>%
    filter(!is.na(value)) %>%
    dplyr::select(-field)
}
dat.out <- 
  stats.tbl %>%
  collect() 
dbWriteTable(conn=res.db, name=PE.cfg$db$stats, value=dat.out, append = TRUE)

#Then the metrics
dat.out <-
  mets.tbl %>%
  collect()
dbWriteTable(conn=res.db, name=PE.cfg$db$metrics, value=dat.out, append = TRUE)

#Fin
dbDisconnect(res.db)
dbDisconnect(src.db)

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
