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

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(drake)
library(callr)
library(PredEng)
library(here)
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
if(interactive() ) {
  pcfg@Decadal <- pcfg@Decadal[2:4]  #Debugging
} 

#'========================================================================
# Setup ####
#'========================================================================
script.complete <- function() {
  list(time = Sys.time(), tempfile = tempfile())
  }

#Setup functions
extract.observations <- function(...) {
  rscript(here("src/B.Extract/C1.HadISST_data.r"))
  script.complete()
}

extract.decadal <- function(datsrc.name) {
  rscript(here('src/B.Extract/D1.Decadal_extraction.r'),
          cmdargs=datsrc.name)
  script.complete()
}

extract.models <- function(...) {
  script.complete()
}

calibration.scripts <- function(...) {
  calib.scripts <- dir(here("src/C.Calibrate/"),pattern=".r$",full.names = TRUE)
  for(scp in calib.scripts) {
    rscript(scp)
  }
  script.complete()
}

stat.jobs <- function(...){
  rscript(here("src/D.Statistics/A1.Partition_stats.r"))
  return(seq(readRDS(PE.scratch.path(pcfg,"statjoblist"))))
}

process.stat <- function(stat.id) {
  rscript(here("src/D.Statistics/B1.Calculate_stats.r"),
          cmdargs=stat.id)
  script.complete()
}


#Make a plan
the.plan <-  
  drake_plan(
    Observations=target(command=extract.observations(),
                        trigger=trigger(change=pcfg@Observations)),
    Decadal=target(extract.decadal(datsrc),
                   transform = map(datsrc=!!(names(pcfg@Decadal))),
                   trigger=trigger(change=pcfg@Decadal[[datsrc]])),
    Extractions=target(extract.models(Observations,Decadal),
                    transform=combine(Decadal)),
    Calibration=calibration.scripts(Extractions),
    Statjobs=stat.jobs(Calibration),
    Stats=target(process.stat(Statjobs),
                 dynamic=map(Statjobs)))


#'========================================================================
# And Go ####
#'========================================================================
if(interactive()) {
  #Visualise
  print(vis_drake_graph(the.plan,targets_only=TRUE))
} else {
  #Parallelism
  options(clustermq.scheduler = "lsf",
          clustermq.template=here("src/Y.HPC/HPC_template.tmpl"))

  #Paw Patrol - så er det nu!
  make(the.plan, parallelism = "clustermq", jobs = 4)
  make(the.plan)
}

#Custom cleaning function
clean_regex <- function(regex) {
  clean(list=grep(regex,cached(),value=TRUE))
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
