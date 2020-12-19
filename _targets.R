#'========================================================================
# makefile using targets
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Aug 20 17:03:55 2020
#
# MA makefile using the targets package
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
cat(sprintf("\n%s\n","PredEng makefile"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
suppressPackageStartupMessages({
  library(targets)
  library(callr)
  library(PredEng)
  library(here)
})
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
print(pcfg)

tar_option_set(error = "workspace")
tar.l <- list()

#'========================================================================
# Helper functions ####
#'========================================================================
# Helper functions
ext.script <- function(this.scp,...) {
  this.args <- unlist(list(...))
  log.fname <- 
    file.path(here("_targets","logs"),
              paste(substr(basename(dirname(this.scp)),1,1),
                    gsub("\\.r$","",basename(this.scp)),
                    paste(this.args,collapse="."),
                    sep="."))
  callr::rscript(this.scp,
                 cmdargs=this.args,
                 stdout=log.fname,
                 stderr="2>&1",
                 show=FALSE)
  return(Sys.time())
}

#'========================================================================
# Data Extraction ####
#'========================================================================
#Observations
extract.observations <-
  function(this.src) {
    obs.script <- switch(this.src@name,
                         "EN4"="A2.EN4_extraction.r",
                         "HadISST"="A1.HadISST_data.r",
                         stop("Cannot find observation script"))
    ext.script(here("src/B.Extract/",obs.script),
               this.src@name)
    return(this.src)
  }

tar.l$obs.src <-
  tar_target(obs.src,
             pcfg@Observations)

tar.l$observations <-
  tar_target(observations,
             extract.observations(obs.src))

#Decadal sources
#But only process them if they are defined. The lazy evaluation approach that is
#being employed here seems to handle this ok.
extract.decadal <- 
  function(this.src) {
    ext.script(here('src/B.Extract/D1.Decadal_extraction.r'),
               this.src@name)
    return(this.src)
  }

calc.realmeans <- function(this.src) {
  ext.script(here("src/B.Extract/Z1.Calculate_realmeans.r"),
             this.src@type,this.src@name)
  return(this.src)}

if(length(pcfg@Decadal)!=0 & !pcfg@obs.only){
  tar.l$decadal.srcs <-
    tar_target(decadal.srcs,
               pcfg@Decadal,
               iteration = "list")
  
  tar.l$extract.decadal <-
    tar_target(decadal.extr,
               extract.decadal(decadal.srcs),
               pattern=map(decadal.srcs),
               iteration="list")

  tar.l$realmean.decadal <-
    tar_target(decadal.realmean,
               calc.realmeans(decadal.extr),
               pattern=map(decadal.extr),
               iteration="list")
} 


#'========================================================================
# Calibration ####
#'========================================================================
#Climatology
climatology.fn <- function(...) {
  ext.script(here("src/C.Calibrate/A1.Climatological_statistics.r"))}


tar.l$clim <-
  tar_target(clim,
             climatology.fn(decadal.realmean, observations))

#Calibration
calibration.fn <- function(...) {
    calib.scripts <- dir(here("src/C.Calibrate"),pattern="^B.*r$",full.names = TRUE)
    for(scp in calib.scripts) {
      ext.script(scp)}
    return(Sys.time())
}

tar.l$calibratioon <-
  tar_target(calibration,
             calibration.fn(clim))


#Ensemble means
ensmean.fn <- function(...) {
  ext.script(here("src/C.Calibrate/C1.Ensemble_means.r"))
}


tar.l$ensmean <-
  tar_target(ensmeans,
             ensmean.fn(calibration))

#'========================================================================
# Stats ####
#'========================================================================
#Get stat jobs to process
stat.jobs.fn <- function(...){
    stats.tbl <- 
      tibble(stat.obj=pcfg@statistics@.Data) %>%
      mutate(st.name=map_chr(stat.obj,slot,"name"),
             st.uses.globalROI=map_lgl(stat.obj,slot,"use.globalROI"))
    stat.sp.comb <-
      stats.tbl %>%
      filter(!st.uses.globalROI) %>%
      pull(st.name) %>%
      expand_grid(stat=.,
                  sp=pcfg@spatial.polygons$name)
    global.stats <- 
      stats.tbl %>%
      filter(st.uses.globalROI) %>%
      pull(st.name) %>%
      tibble(stat=.,sp=PE.cfg$misc$globalROI)
    
    todo.stats <- 
      bind_rows(stat.sp.comb,global.stats) %>%
      relocate(sp)
    return(todo.stats)
}

tar.l$stat.jobs <-
  tar_target(statJobs,
             stat.jobs.fn(pcfg,ensmeans))

#Process stats
process.stat <- function(this) {
    ext.script(here("src/D.Statistics/B1.Calculate_stats.r"),
                this$sp,this$stat)
}
tar.l$stats <-
  tar_target(stats,
             process.stat(statJobs),
             pattern=map(statJobs))

#Calculation verification metrics
#But only if we have been running something other than observations
process.metrics <- function(...){
    ext.script(here("src/D.Statistics/C1.Calculate_scalar_skill_metrics.r"))
}

if(!pcfg@obs.only) {
  tar.l$metrics <-
    tar_target(metrics,
               process.metrics(stats))
}

#'========================================================================
# Outputs ####
#'========================================================================
#Pointwise extraction
pointwise.fn<- function(...){
  ext.script(here("src/ZZ.Helpers/A1.Pointwise_extraction.r"))
}

tar.l$pointwise <-
  tar_target(pointwise,
             pointwise.fn(metrics,stats))  #Stats is a included as a second dependency for cases when
                                           #there are no metrics to calculate

#Simplified results table
results.table <- function(...){
  ext.script(here("src/ZZ.Helpers/Extract_results_from_SQlite.r"))
}

tar.l$tbl <-
  tar_target(results.table,
             results.table(pointwise))

#Markdown report
make.report <- function(...){
  ext.script(here("src/D.Statistics/C2.Visualise_scalar_skill_metrics.r"))
}

tar.l$report <-
  tar_target(report,
             make.report(results.table))

#'========================================================================
# Make a Plan! ####
#'========================================================================

#Make it all into a plan
pline <- tar_pipeline(tar.l)
rm(tar.l)
pline


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
