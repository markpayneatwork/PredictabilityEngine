#'========================================================================
# make
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Aug 20 17:03:55 2020
#
# Make functionality for PredEng using drake
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
cat(sprintf("\n%s\n","make"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

library(drake)
library(callr)
library(PredEng)
library(here)
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
<<<<<<< HEAD
if(interactive() ) {
#  pcfg@Decadal <- pcfg@Decadal[2:4]  #Debugging
} 
=======
>>>>>>> 8555bd728f97e14188ecaac908aad34f6c9769b9

#'========================================================================
# Setup ####
#'========================================================================
# Helper functions
script.complete <- function() {
  list(time = Sys.time(), tempfile = tempfile())
}

log.file <- function(...) {
  file.path(PE.scratch.path(pcfg,"logs"),sprintf(...))
}


#Setup functions
extract.observations <- function(...) {
  obs.script <- switch(pcfg@Observations@name,
                      "EN4"="C2.EN4_extraction.r",
                      "HadISST"="C1.HadISST_data.r",
                      stop("Cannot find observation script"))
  callr::rscript(here("src/B.Extract/",obs.script),
                 stdout=log.file("B.Observations.%s",pcfg@Observations@name),
                 stderr=log.file("B.Observations.%s",pcfg@Observations@name))
script.complete()
}

extract.decadal <- function(datsrc.name) {
  callr::rscript(here('src/B.Extract/D1.Decadal_extraction.r'),
                 cmdargs=datsrc.name,
                 stdout=log.file("B.Decadal.%s",datsrc.name),
                 stderr=log.file("B.Decadal.%s",datsrc.name))
  script.complete()
}

extract.models <- function(...) {
  script.complete()
}

calibration.scripts <- function(...) {
  calib.scripts <- dir(here("src/C.Calibrate/"),pattern=".r$",full.names = TRUE)
  for(scp in calib.scripts) {
    callr::rscript(scp,
                   stdout=log.file("C.%s",basename(scp)),
                   stderr=log.file("C.%s",basename(scp)))
  }
  script.complete()
}

stat.jobs <- function(...){
  callr::rscript(here("src/D.Statistics/A1.Partition_stats.r"))
  return(seq(readRDS(PE.scratch.path(pcfg,"statjoblist"))))
}

process.stat <- function(stat.id) {
  callr::rscript(here("src/D.Statistics/B1.Calculate_stats.r"),
          cmdargs=stat.id,
          stdout=log.file("D.Stats.%03i",stat.id),
          stderr=log.file("D.Stats.%03i",stat.id))
  script.complete()
}


#Make a plan
the.plan <-  
  drake_plan(
<<<<<<< HEAD
#    Observations=target(command=extract.observations(),
 #                       trigger=trigger(change=pcfg@Observations)),
    Decadal=target(extract.decadal(datsrc),
                   transform = map(datsrc=!!(names(pcfg@Decadal))),
                   trigger=trigger(change=pcfg@Decadal[[datsrc]])),
#    Extractions=target(extract.models(Observations,Decadal),
    Extractions=target(extract.models(Decadal),
=======
    Observations=target(command=extract.observations(),
                        trigger=trigger(command=FALSE,change=pcfg@Observations)),
    Decadal=target(extract.decadal(datsrc),
                   transform = map(datsrc=!!(names(pcfg@Decadal))),
                   trigger=trigger(command=FALSE,change=pcfg@Decadal[[datsrc]])),
    Extractions=target(extract.models(Observations,Decadal),
>>>>>>> 8555bd728f97e14188ecaac908aad34f6c9769b9
                       transform=combine(Decadal)),
    Calibration=calibration.scripts(Extractions),
    Statjobs=stat.jobs(Calibration),
    Stats=target(process.stat(Statjobs),
                 dynamic=map(Statjobs)))

<<<<<<< HEAD
=======

#'========================================================================
# And Go ####
#'========================================================================
#Set parallelism
options(clustermq.scheduler = "multicore")

#Paw Patrol - så er det nu!
make(the.plan, parallelism = "clustermq", jobs = 4,keep_going=TRUE)

>>>>>>> 8555bd728f97e14188ecaac908aad34f6c9769b9
#'========================================================================
# Supplementary ####
#'========================================================================
#Visualise
vis <- function() print(vis_drake_graph(the.plan,targets_only=TRUE))
if(interactive()) {
	vis()
}

#Custom cleaning function
clean_regex <- function(regex) {
  clean(list=grep(regex,cached(),value=TRUE))
}

#'========================================================================
# And Go ####
#'========================================================================
#Set parallelism
options(clustermq.scheduler = "multicore")

#Paw Patrol - så er det nu!
make(the.plan, parallelism = "clustermq", jobs = 8)
#make(the.plan)


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
