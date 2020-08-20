#'========================================================================
# Automated_run
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Aug 20 13:37:16 2020
#
# Performs an automated run through all key steps
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
cat(sprintf("\n%s\n","Automated_run"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
library(PredEng)
library(here)
library(callr)
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configuration ####
#'========================================================================
#Don't run all extractions
decadal.idxs <- seq(pcfg@Decadal)
decadal.idxs <- 3:4

#'========================================================================
# Setup ####
#'========================================================================
separator.line <- function() {
  log_msg("\n%s\n",paste(rep("-",60),collapse=""))
}

source.this <- function(scp) {
  separator.line()
  source(scp)
}

#'========================================================================
# And Go ####
#'========================================================================
#Observations extraction
#source.this(here('src/B.Extract/C1.HadISST_data.r'))

#Loop over Decadal options
for(i in decadal.idxs) {
#  rscript(here('src/B.Extract/D1.Decadal_extraction.r'),
 #         cmdargs=sprintf("%i",i))
  do.this <- i
  source.this(here('src/B.Extract/D1.Decadal_extraction.r'))
  separator.line()
}

#Calibration
calib.scripts <- dir(here("src/C.Calibrate/"),pattern=".r$",full.names = TRUE)
for(scp in calib.scripts) {
  source.this(scp)
}

#Statistics
stat.scripts <- dir(here("src/D.Statistics/"),pattern=".r$",full.names = TRUE)
for(scp in stat.scripts) {
  source.this(scp)
}

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
