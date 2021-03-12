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
#pcfg@Decadal[[1]]@sources <- head(pcfg@Decadal[[1]]@sources,2)

#'========================================================================
# Helper functions ####
#'========================================================================
# Helper functions
ext.script <- function(this.scp,...,args=NULL) {
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

#'========================================================================
# Data Extraction ####
#'========================================================================
#Observations
tar.l$obs.src <-
  tar_target(obs.src,
             pcfg@Observations)

tar.l$observations <-
  tar_target(observations,
             {obs.script <- switch(obs.src@name,
                                   "EN4"="A2.EN4_extraction.r",
                                   "HadISST"="A1.HadISST_data.r",
                                   "HadSLP2"="A3.HadSLP2.r",
                                   stop("Cannot find observation script"))
             ext.script(here("src/B.Extract/",obs.script),
                        args=c(srcType=obs.src@type,
                               srcName=obs.src@name))})

#Model sources
tar.l$model.srcs <-
  tar_target(model.srcs,
             pcfg@Models,
             iteration = "list")

tar.l$model.extracts <-
  tar_target(model.extracts,
             ext.script(here('src/B.Extract/B1.CDO_based_extraction.r'),
                        model.srcs,
                        args=c(srcType=model.srcs@type,
                               srcName=model.srcs@name)),
             pattern=map(model.srcs))



#Realisation means
tar.l$model.realmeans <-
  tar_target(model.realmeans,
             ext.script(here("src/C.Calibrate/A1.Calculate_realmeans.r"),
                        model.extracts,
                        args=c(srcType=model.extracts$srcType,
                               srcName=model.extracts$srcName)),
             pattern=map(model.extracts))

#'========================================================================
# Calibration ####
#'========================================================================
#Extraction databases
#We merge in external extractions at this point
get.extraction.databases <- function(object,...) {
  tibble(path=PE.db.list(object,PE.cfg$db$extract),
         checksum=tools::md5sum(path),
         datetime=file.info(path)$mtime,
         srcTypeName=gsub("^.*_Extraction_(.*).sqlite$","\\1",basename(path))) %>%
    separate(srcTypeName,
             c("srcType","srcName"),
             sep="_",
             extra="drop")
}

tar.l$extraction.databases <-
  tar_target(extraction.databases,
             get.extraction.databases(pcfg,model.realmeans),
             cue=tar_cue("always"))


tar.l$all.extracts <-
  tar_target(all.extracts,
             bind_rows(observations,extraction.databases))

#Climatology
tar.l$clim <-
  tar_target(clim,
             ext.script(here("src/C.Calibrate/A2.Climatological_statistics.r"),
                        all.extracts,
                        args=c(srcType=all.extracts$srcType,
                               srcName=all.extracts$srcName)),
             pattern=map(all.extracts))

#Calibration
tar.l$calibration <-
  tar_target(calibration,
             ext.script(here("src/C.Calibrate/B1.Mean_adjustment.r"),
                        clim,observations,
                        args=c(srcType=clim$srcType,
                               srcName=clim$srcName)),
             pattern=map(clim))
# 
# if(any(pcfg@calibrationMethods=="NAOmatching")) {
#   tar.l$NAOmatching <- 
#     tar_target(NAOmatching,
#                ext.script(here("src/C.Calibrate/B2.NAO_matching.r"),
#                           calibration))
# }
# 
#Ensemble means
tar.l$ensmean.l <-
  tar_target(ensmean.src,
             calibration %>%
               filter(srcType!="Observations") %>%
               nest(members=c(-srcType)))

tar.l$ensmean <-
  tar_target(ensmeans,
             ext.script(here("src/C.Calibrate/C1.Ensemble_means.r"),
                        args=c(srcType=ensmean.src$srcType,
                               srcName="ensmean")),
             pattern=map(ensmean.src))

tar.l$stat.srcs <-
  tar_target(stat.srcs,
             bind_rows(ensmeans,calibration))

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
             ext.script(here("src/D.Statistics/B1.Calculate_stats.r"),
                        args=c(spName=statJobs$spName,
                               statName=statJobs$statName,
                               srcType=stat.srcs$srcType,
                               srcName=stat.srcs$srcName)),
             pattern=cross(statJobs,stat.srcs))

#Rolling means
tar.l$rollmean <-
  tar_target(rollmean,
             ext.script("src/D.Statistics/B2.Rolling_means.r",
                        stats))

#Calculation verification metrics
tar.l$scalar.metrics <-
  tar_target(scalar.metrics,
             ext.script(here("src/D.Statistics/C1.Calculate_scalar_skill_metrics.r"),
                        stats,rollmean))

tar.l$field.metrics <-
    tar_target(field.metrics,
               ext.script(here("src/D.Statistics/C2.Calculate_field_skill_metrics.r"),
                          stats,rollmean))

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
#              ext.script(here("src/E.Postprocessing/A1.Pointwise_extraction.r"),
#                         points,stats))  #Stats is a included as a second dependency for cases when
# #there are no metrics to calculate

#Markdown report
tar.l$report <-
  tar_target(report,
             ext.script(here("src/E.Postprocessing/B1.Visualise_scalar_skill_metrics.r"),
                        scalar.metrics))



#'========================================================================
# Make a Plan! And then change it. ####
#'========================================================================
#Synchronise list and target names
names(tar.l) <- map_chr(tar.l,~get("name",envir=.x$settings))

#If only running obs, reduce the number of targets
if(pcfg@obs.only) {
  obs.only.drop <-
    c("report","scalar.metrics","field.metrics",
      "ensmean.src","ensmeans",
      "model.srcs","model.extracts","model.realmeans","extraction.databases")
  tar.l <- tar.l[!(names(tar.l) %in% obs.only.drop)]
}

#Turn off field metrics if there aren't any
if(!any(map_lgl(pcfg@statistics,returns.field))) {
  tar.l <- tar.l[(names(tar.l)!="field.metrics")]
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
