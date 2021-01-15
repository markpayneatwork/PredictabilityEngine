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
pcfg <- PE.load.config()

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
ext.script <- function(this.scp,...,args=NULL) {
  log.fname <- 
    file.path(here("_targets","logs"),
              paste(substr(basename(dirname(this.scp)),1,1),
                    gsub("\\.r$","",basename(this.scp)),
                    paste(args,collapse="."),
                    sep="."))
  callr::rscript(this.scp,
                 cmdargs=args,
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
                         "HadSLP2"="A3.HadSLP2.r",
                         stop("Cannot find observation script"))
    ext.script(here("src/B.Extract/",obs.script),
               args=this.src@name)
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
               args=this.src@name)
    return(this.src)
  }

calc.realmeans <- 
  function(this.src) {
  ext.script(here("src/B.Extract/Z1.Calculate_realmeans.r"),
             args=c(this.src@type,this.src@name))
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
tar.l$clim <-
  tar_target(clim,
             ext.script(here("src/C.Calibrate/A1.Climatological_statistics.r"),
                        decadal.realmean, observations))

#Calibration
tar.l$calibratioon <-
  tar_target(calibration,
             ext.script(here("src/C.Calibrate/B1.Mean_adjustment.r"),
                        clim))

if(any(pcfg@calibrationMethods=="NAOmatching")) {
  tar.l$NAOmatching <- 
    tar_target(NAOmatching,
             ext.script(here("src/C.Calibrate/B2.NAO_matching.r"),
                        calibration))
}

#Ensemble means
tar.l$ensmean <-
  tar_target(ensmeans,
             ext.script(here("src/C.Calibrate/C1.Ensemble_means.r"),
                        calibration,NAOmatching))

#'========================================================================
# Stats ####
#'========================================================================
#Get stat jobs to process
stat.jobs.fn <- function(...){
  #Check that we have at least some statistics
  assert_that(length(pcfg@statistics)>0,
              msg="No statistics defined. Must be at least one.")


  #Extract statistics
  todo.stats <- 
    tibble(st=pcfg@statistics@.Data) %>%
    mutate(statName=map_chr(st,slot,"name"),
           st.request=map(st,slot,"spatial.polygons"),
           st.uses.globalROI=map_lgl(st,slot,"use.globalROI"),
           st.request.mt=map_lgl(st.request, ~length(.x)==0),
           st.request.na=map_lgl(st.request, ~any(is.na(.x))),
           #Merge in defaults
           spName=map_if(st.request,st.request.mt, ~ pcfg@spatial.polygons$name),
           spName=map_if(spName,st.request.na,~ NULL),
           spName=map_if(spName,st.uses.globalROI, ~ c(.x,PE.cfg$misc$globalROI))) %>% 
    unnest(spName) 


  return(todo.stats)
}

tar.l$stat.jobs <-
  tar_target(statJobs,
             stat.jobs.fn(pcfg))

#Process stats
stat.fn <- function(this.stat,...) {
  ext.script(here("src/D.Statistics/B1.Calculate_stats.r"),
             args=c(this.stat$spName,this.stat$statName))
}

tar.l$stats <-
  tar_target(stats,
             stat.fn(statJobs,ensmeans),
             pattern=map(statJobs))

#Calculation verification metrics
tar.l$metrics <-
  tar_target(metrics,
             ext.script(here("src/D.Statistics/C1.Calculate_scalar_skill_metrics.r"),
                        stats))

#'========================================================================
# Outputs ####
#'========================================================================
#Pointwise extraction
tar.l$pointwise <-
  tar_target(pointwise,
             ext.script(here("src/E.Postprocessing/A1.Pointwise_extraction.r"),
                        metrics,stats))  #Stats is a included as a second dependency for cases when
                                         #there are no metrics to calculate

#Simplified results table
tar.l$tbl <-
  tar_target(results.table,
             ext.script(here("src/E.Postprocessing/Extract_results_from_SQlite.r"),
                        pointwise))

#Markdown report
if(!pcfg@obs.only) {
  tar.l$report <-
    tar_target(report,
               ext.script(here("src/E.Postprocessing/B1.Visualise_scalar_skill_metrics.r"),
                          results.table))
}

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
