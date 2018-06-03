#'========================================================================
# I2. Collate spacetime extractions
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Jun  1 15:53:49 2018
#
# Collates the results of spacetime extractions
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
cat(sprintf("\n%s\n","I2. Collate spacetime extractions"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tibble)
library(dplyr)
library(reshape2)
library(lubridate)
load("objects/configuration.RData")
  
#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.debug.level(0)  #Non-zero lets us run with just a few points
} else {
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}

#Directory setup
base.dir <- pcfg@scratch.dir
res.dir <- define_dir(base.dir,"pointwise")

#'========================================================================
# Select input data ####
#'========================================================================
#Get list of results files
res.fnames <- dir(res.dir,full.names = TRUE)

#Loop over them individually and import
res.l <- list()
for(f in res.fnames){
  var.names <- load(f)
  res.l[[f]]   <- get(var.names)
}

#Now we have one big list of results data. As previously, merging directly
#might cause some problems. Instead, we keep the bare minimums
keep.cols <- c("name","start.date","date","ID","value")
res.l <- lapply(res.l,"[",keep.cols)

#Merge into one big object and add meta information
matchup.res <- bind_rows(res.l) %>% 
  mutate(lead=as.numeric(round(difftime(date,start.date,units="days") /15)*15))

#'========================================================================
# Complete ####
#'========================================================================
#Save results
extract.spdf <- pcfg@spacetime.extraction
save(matchup.res,extract.spdf,file=file.path(base.dir,"Pointwise_extraction.RData"))

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
