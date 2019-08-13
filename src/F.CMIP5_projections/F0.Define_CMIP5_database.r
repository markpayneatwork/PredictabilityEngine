#'========================================================================
# F0.Define_CMIP5_database
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Tue Aug 13 11:10:31 2019
#
# Analyses the CMIP5 database and defines its contents, based on a common 
# set of constraints
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
cat(sprintf("\n%s\n","F0.Define_CMIP5_database"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)
library(PredEng)

#'========================================================================
# Configure ####
#'========================================================================
# Put the configuration here, so that it's easier to find.
config.arg.l <- list(
  var.constraints = c("tos","so"),
  expt.constraints = c("historical","rcp26","rcp85"),
  start.yr=1950,
  end.yr=2100)

#'========================================================================
# Setup CMIP5 models ####
# CMIP5 setup is probably easiest done via a configuration function that
# takes a set of arguments
#'========================================================================
constrain.CMIP5 <- function(var.constraints,
                            expt.constraints,
                            start.yr,
                            end.yr,  
                            r=NA,i=1,p=1,
                            meta="missing") {
  
  #Get list of files (if a metadata database is not supplied)
  if(missing("meta")) {
    CMIP5.meta <- CMIP5_meta(file.path(PE.cfg$dirs$datasrc,"CMIP5"))
  } else {
    CMIP5.meta <- meta
  }
  
  #Debugging configuration
  # var <- "so"
  # var.constraints <- c("tos","so")
  # expt.constraints <- c("historical","rcp26","rcp85")
  # start.yr <- 1960
  # end.yr <- 2100
  # r <- NA
  # i <- 1
  # p <- 1

  #Apply raw filtering
  #Select variable and experiment
  CMIP5.sel <- filter(CMIP5.meta,
                      variable%in%var.constraints,
                      experiment %in% expt.constraints)
  
  #Apply start and end-time constraints to remove unnecessary files from the
  #processing chain e.g. a historical file running from 1860-1870
  CMIP5.sel <- filter(CMIP5.sel,
                      year(start.date) <= end.yr,
                      year(end.date) >= start.yr)
  
  
  #Check for perturbed physics runs and differing initialisation methods
  #Generally, I don't know how to interpret these, so we just drop them
  if(!is.na(r)) {
    CMIP5.sel <- filter(CMIP5.sel,real.r%in%r)
  }
  if(!is.na(i)) {
    CMIP5.sel <- filter(CMIP5.sel,real.i%in%i)
  }
  if(!is.na(p)) {
    CMIP5.sel <- filter(CMIP5.sel,real.p%in%p)
  }
  
  #Now apply the consistency constraints
  #We want to have a (relatively) consistent set of data all the way through
  #By this, we mean that for all we models, we have
  #1. the same set of experiments for all variables
  #2. full time coverage, from start.yr to end.yr inclusive
  #3. a consistent number of realizations during this time, so that the sample size is constant
  
  #First, filter by the variable-experiment constraint (c1)
  #Get the list of unique combinations required for each model, then filter
  var.expt.combs <- expand.grid(var=var.constraints,expt=expt.constraints) %>%
    unite(var.expt,var,expt) 
  CMIP5.constrain <- unite(CMIP5.sel,var.expt,variable,experiment,remove=FALSE) %>%
    group_by(model) %>%
    filter(all(var.expt.combs$var.expt %in% var.expt)) %>%
    dplyr::select(-var.expt)
  
  #I will omit the remaining constraints for the moment, but we should check this manually
  #using e.g. CMIP5_vis
  
  #Now do selection by models that run at least all the way to the end year
  # if(!is.na(end.yr)) {
  #   #Find model-expt-realization combinations to retain
  #   mdl.expt.max.yr <- CMIP5.sel %>%
  #     filter(experiment!="historical") %>%
  #     group_by(model,experiment,realization) %>%
  #     summarise(last.year=max(year(end.date))) %>%
  #     unite(key,model,experiment,realization,remove=FALSE)
  #   retain.these <- filter(mdl.expt.max.yr,last.year>=end.yr)
  #   retain.key <- c(retain.these$key,
  #                   with(retain.these,paste(model,"historical",realization,sep="_")))
  #   
  #   #Apply filter
  #   CMIP5.sel <- unite(CMIP5.sel,key,model,experiment,realization,remove=FALSE) %>%
  #     filter(key %in% retain.key) %>%
  #     dplyr::select(-key)
  # }

  return(CMIP5.constrain)
}

#Extract a common database for use in reference runs
CMIP5.db <- do.call(constrain.CMIP5,config.arg.l)

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
saveRDS(CMIP5.db,file="objects/CMIP5db.rds")

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
