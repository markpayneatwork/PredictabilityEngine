#'========================================================================
# C0.Extract_observations
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Tue Jun  4 10:12:04 2019
#
# Control script that switches between observations depending on the configuration.
# Intended to be used primarily by the Makefile
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
cat(sprintf("\n%s\n","C0.Extract_observations"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tidyverse)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Setup ####
#'========================================================================
#Observation source
obs.src <- switch(pcfg@Observations@name,
                  "EN4"="src/C.Observations/C1.EN4_data_extraction.r",
                  "HadISST"="src/C.Observations/C1.HadISST_data.r",
                  stop(paste0('Observations of type "',
                             pcfg@Observations@name,
                             '" are not supported.')))

#'========================================================================
# And Go ####
#'========================================================================
source(obs.src)

#And then persistence
source("src/C.Observations/C2.Persistence.r")

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
