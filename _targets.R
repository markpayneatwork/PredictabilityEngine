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
  library(assertthat)
})
pcfg <- PE.load.config()

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
PE.config.summary(pcfg)

tar_option_set(error = "workspace")
tar.l <- list()

#'========================================================================
# Helper functions ####
#'========================================================================
# Helper functions
run.extern.script <- 
  function(this.scp,...,args=NULL) {
    assert_that(!is.null(names(args)) | is.null(args),msg="args variable must be a named vector")
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
    return(bind_cols(tibble(script=this.scp,
                            finished=Sys.time()),
                     as.list(args)))
  }

#Links targets without evaluating them
lazy.collator.fn <- function(...) {return(NULL)}

#'========================================================================
# Data Extraction ####
#'========================================================================
#Choose extraction script
run.extraction.script <- function(this.src) {
  this.extract.script <- 
    switch(paste(this.src@type,this.src@name,sep="/"),
           "Observations/EN4"="A2.EN4_extraction.r",
           "Observations/HadISST"="A1.HadISST_data.r",
           "Observations/HadSLP2"="A3.HadSLP2.r",
           "B1.CDO_based_extraction.r")
  run.extern.script(here("src/B.Extract/",this.extract.script),
             args=c(srcType=this.src@type,
                    srcName=this.src@name))
}

#Reference model
tar.l$reference.src <-
  tar_target(reference.src,
             pcfg@reference)

#Data sources
#Only run if we need to do some local extraction
tar.l$data.srcs <-
  tar_target(datasrcs,
             pcfg@data.sources,
             iteration = "list")

tar.l$datasrc.extracts <-
  tar_target(datasrc.extracts,
             run.extraction.script(datasrcs),
             pattern=map(datasrcs))

tar.l$datasrc.realmeans <-
  tar_target(datasrc.realmeans,
             run.extern.script(here("src/B.Extract/Z1.Calculate_realmeans.r"),
                        datasrc.extracts,
                        args=c(srcType=datasrc.extracts$srcType,
                               srcName=datasrc.extracts$srcName)),
             pattern=map(datasrc.extracts))

tar.l$local.data <-
  tar_target(local.data,
             datasrc.realmeans)

#'========================================================================
# Calibration ####
#'========================================================================
#Extraction databases
#We merge in external extractions at this point
get.extraction.databases <- function(object,...) {
  tibble(path=PE.db.list(object,PE.cfg$db$extract),
         checksum=tools::md5sum(path),
         datetime=file.info(path)$mtime) %>%
  extract(path,into="srcTypeName",regex="^.*_Extraction_(.*).sqlite$",remove = FALSE) %>%
  separate(srcTypeName,
             c("srcType","srcName"),
             sep="_",
             extra="drop")
}

tar.l$extraction.databases <-
  tar_target(extraction.databases,
             get.extraction.databases(pcfg),
             cue=tar_cue("always"))

#Climatology
tar.l$clim <-
  tar_target(clim,
             run.extern.script(here("src/C.Calibrate/A1.Climatological_statistics.r"),
                               extraction.databases,
                               args=c(srcType=extraction.databases$srcType,
                                      srcName=extraction.databases$srcName)),
             pattern=map(extraction.databases))

#Calibration
tar.l$calibration <-
  tar_target(calibration,
             run.extern.script(here("src/C.Calibrate/B1.Mean_adjustment.r"),
                        clim, reference.src,
                        args=c(srcType=clim$srcType,
                               srcName=clim$srcName)),
             pattern=map(clim))
# 
# if(any(pcfg@calibrationMethods=="NAOmatching")) {
#   tar.l$NAOmatching <- 
#     tar_target(NAOmatching,
#                run.extern.script(here("src/C.Calibrate/B2.NAO_matching.r"),
#                           calibration))
# }
# 

#Ensemble and Grand means
if(pcfg@obs.only) {
  tar.l$stat.srcs <-
    tar_target(stat.srcs,
               calibration)
} else {
  tar.l$ensmean.l <-
    tar_target(ensmean.src,
               calibration %>%
                 filter(srcType!="Observations") %>%
                 nest(members=c(-srcType)))
  
  tar.l$ensmean <-
    tar_target(ensmeans,
               run.extern.script(here("src/C.Calibrate/C1.Ensemble_means.r"),
                                 args=c(srcType=ensmean.src$srcType,
                                        srcName="ensmean")),
               pattern=map(ensmean.src))
  
  tar.l$GrandEns <-
    tar_target(grandens,
               run.extern.script(here("src/C.Calibrate/C2.Grand_ensemble.r"),
                                 args=c(srcType=ensmean.src$srcType,
                                        srcName="grandens")),
               pattern=map(ensmean.src))

  tar.l$stat.srcs <-
    tar_target(stat.srcs,
               bind_rows(ensmeans,grandens,calibration)) 
}
  
#'========================================================================
# Stats ####
#'========================================================================
#Get stat jobs to process
stat.jobs.fn <- function(...){
  #Check that we have at least some statistics
  assert_that(length(pcfg@statistics)>0,
              msg="No statistics defined. Must be at least one.")

  #Extract statistics
  these.stats <- 
    tibble(st=pcfg@statistics@.Data) %>%
    mutate(statName=map_chr(st,slot,"name"),
           st.uses.globalROI=map_lgl(st,slot,"use.globalROI"),
           st.request=map(st,slot,"spatial.polygons"),
           st.request.is.mt=map_lgl(st.request, ~length(.x)==0),
           st.request.is.na=map_lgl(st.request, ~any(is.na(.x))),
           #Set spatial polygons by merging in defaults
           spName=map_if(st.request,st.request.is.mt, ~ pcfg@spatial.polygons$name),
           spName=map_if(spName,st.request.is.na,~ NULL),
           spName=map_if(spName,st.uses.globalROI, ~ c(.x,PE.cfg$misc$globalROI))) %>%
    unnest(spName)
  
  return(these.stats)
}

tar.l$stat.jobs <-
  tar_target(statJobs,
             stat.jobs.fn(pcfg))

#Process stats
tar.l$stats <-
  tar_target(stats,
             run.extern.script(here("src/D.Statistics/B1.Calculate_stats.r"),
                        args=c(spName=statJobs$spName,
                               statName=statJobs$statName,
                               srcType=stat.srcs$srcType,
                               srcName=stat.srcs$srcName)),
             pattern=cross(statJobs,stat.srcs))

#Rolling means
tar.l$rollmean <-
  tar_target(rollmean,
             run.extern.script("src/D.Statistics/B2.Rolling_means.r",
                        stats))

#Calculation skill metrics
tar.l$comp.years <-
  tar_target(comp.years,
             pcfg@comp.years)

tar.l$scalar.metrics <-
  tar_target(scalar.metrics,
             run.extern.script(here("src/D.Statistics/C1.Calculate_scalar_skill_metrics.r"),
                        stats,rollmean,comp.years))

tar.l$field.metrics <-
    tar_target(field.metrics,
               run.extern.script(here("src/D.Statistics/C2.Calculate_field_skill_metrics.r"),
                          stats,rollmean,comp.years))

#'========================================================================
# Outputs ####
#'========================================================================
#Pointwise extraction
# tar.l$points <-
#   tar_target(points,
#              slot(pcfg,"pt.extraction"))
# 
# tar.l$pointwise <-
#   tar_target(pointwise,
#              run.extern.script(here("src/E.Postprocessing/A1.Pointwise_extraction.r"),
#                         points,stats))  #Stats is a included as a second dependency for cases when
# #there are no metrics to calculate

#Markdown report
tar.l$report <-
  tar_target(report,
             run.extern.script(here("src/E.Postprocessing/B1.Visualise_scalar_skill_metrics.r"),
                        scalar.metrics))

#Final target of this branch

tar.l$analysis <-
  tar_target(analysis,
             lazy.collator.fn(report,field.metrics))

#'========================================================================
# Make a Plan! And then change it. ####
#'========================================================================
#Synchronise list and target names
names(tar.l) <- map_chr(tar.l,~get("name",envir=.x$settings))

#If only running obs, reduce the number of targets
#Note that we can also have observations in the Model slots though.
if(pcfg@obs.only) {
  obs.only.drop <-
    c("report","scalar.metrics","field.metrics")
  tar.l <- tar.l[!(names(tar.l) %in% obs.only.drop)]
}

#Turn off multimodel aspects if there are no models
if(length(pcfg@data.sources)==0) {
  no.models.drop <-
    c("datasrcs","datasrc.extracts","datasrc.realmeans")
  tar.l <- tar.l[!(names(tar.l) %in% no.models.drop)]
}

#Turn off field metrics if there aren't any
if(!any(map_lgl(pcfg@statistics,slot,"do.field.metrics"))) {
  tar.l <- tar.l[(names(tar.l)!="field.metrics")]
}

#Turn off scalar metrics if there aren't any
if(!any(map_lgl(pcfg@statistics,returns.scalar))) {
  tar.l <- tar.l[!(names(tar.l) %in% c("scalar.metrics","report"))]
}

#Done!
tar.l


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
