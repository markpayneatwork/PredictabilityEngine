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
#Take input arguments, if any
if(interactive()) {
  n.cores <- 8
} else {
  n.cores <-   as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))
}
log_msg("Running with %i cores...\n",n.cores)

obs.only <- TRUE

#'========================================================================
# Setup ####
#'========================================================================
# Helper functions
script.complete <- function() {
  list(time = Sys.time(), tempfile = tempfile())
}

log.file <- function(...) {
  ignore({
    file.path(PE.scratch.path(pcfg,"logs"),sprintf(...))
  })
}


# Extraction ----------------------------------------------------------------------
extract.observations <- function(...) {
  ignore({
    obs.script <- switch(pcfg@Observations@name,
                         "EN4"="A2.EN4_extraction.r",
                         "HadISST"="A1.HadISST_data.r",
                         stop("Cannot find observation script"))
    callr::rscript(here("src/B.Extract/",obs.script),
                   stdout=log.file("B.Observations.%s",pcfg@Observations@name),
                   stderr=log.file("B.Observations.%s",pcfg@Observations@name))
    script.complete()
  })
}

extract.source.list <- function(srcType,srcName) {
  ignore({
    tibble(this.datasrc=slot(pcfg,srcType)@.Data)  %>%
      mutate(this.srcName=map_chr(this.datasrc,slot,"name"),
             this.srcType=map_chr(this.datasrc,slot,"type"),
             sources=map(this.datasrc,slot,"sources")) %>%
      filter(this.srcName %in% srcName) %>%
      unnest(sources)
  })
}

extract.decadal <- ignore(code_to_function(here('src/B.Extract/D1.Decadal_extraction.r')))


# Calibration ---------------------------------------------------------------------
calc.realmeans <- ignore(code_to_function(here("src/B.Extract/Z1.Calculate_realmeans.r")))

calibration.scripts <- function(...) {
  ignore({
    calib.scripts <- dir(here("src/C.Calibrate/"),pattern=".r$",full.names = TRUE)
    for(scp in calib.scripts) {
      callr::rscript(scp,
                     stdout=log.file("C.%s",basename(scp)),
                     stderr=log.file("C.%s",basename(scp)))
    }
    script.complete()
  })
}

# Stats ---------------------------------------------------------------------------
stat.jobs <- function(...){
  ignore({
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
  })
}

process.stat <- function(this) {
  ignore({
    #Now call the script
    callr::rscript(here("src/D.Statistics/B1.Calculate_stats.r"),
                   cmdargs=c(this$sp,this$stat,n.cores),
                   stdout=log.file("D.Stats.%s.%s",this$sp,this$stat),
                   stderr=log.file("D.Stats.%s.%s",this$sp,this$stat))
    return(this)
  })
}

process.metrics <- function(...){
  ignore({
    callr::rscript(here("src/D.Statistics/C1.Calculate_scalar_skill_metrics.r"),
                   stdout=log.file("D.C1.metrics"),
                   stderr=log.file("D.C1.metrics"))
    script.complete()
  })
}

# Outputs -------------------------------------------------------------------------


#'========================================================================
# Make a Plan! ####
#'========================================================================
#Debugging
# extract.decadal <- function(...) {script.complete()}
# extract.observations <- function(...) {script.complete()}
# process.stat <- function(...) {script.complete()}
#calc.realmeans <- function(...) {return(list(...))}

end.game <-
  drake_plan(Observations=target(command=extract.observations(),
                                 trigger=trigger(command=FALSE,
                                                 change=pcfg@Observations)),
             Calibration=target(calibration.scripts(Extractions),
                                trigger=trigger(command=FALSE)),
             StatJobs=target(stat.jobs(Calibration),
                             trigger=trigger(command=FALSE,
                                             change=pcfg@statistics)),
             Stats=target(process.stat(StatJobs),
                          dynamic=map(statjob=StatJobs),
                          trigger=trigger(command=FALSE),
                          hpc=FALSE),
             trace=TRUE)

if(!pcfg@obs.only) {
  dec.plan <- 
    drake_plan(DSrc=target(extract.source.list(srcType="Decadal",
                                               srcName=decadalSrcName),
                           transform = map(decadalSrcName=!!names(pcfg@Decadal)),
                           trigger=trigger(command=FALSE,
                                           change=pcfg@Decadal[[decadalSrcName]])),
               DExtr=target(extract.decadal(DSrc),
                            transform=map(DSrc),
                            dynamic=map(DSrc),
                            trigger=trigger(command=FALSE)),
               DRealmeans=target(calc.realmeans(srcType="Decadal",
                                                srcName=decadalSrcName,
                                                DExtr),
                                 transform=map(DExtr)),
               DFrags=target(c(DRealmeans),
                             transform=combine(DRealmeans)),
               Extractions=target(list(Observations,DFrags),
                                  trigger=trigger(command=FALSE)),
               Metrics=target(process.metrics(Stats),
                              hpc=FALSE),
               trace=TRUE)
  vis_drake_graph(dec.plan,targets_only = TRUE)             
  the.plan <- bind_plans(dec.plan,end.game)
} else {
  
  obs.extractions <- 
    drake_plan(Extractions=target(list(Observations),
                     trigger=trigger(command=FALSE)))
  
  the.plan <- bind_plans(obs.extractions,end.game)
}


#'========================================================================
# Check it twice ####
#'========================================================================
#Visualise
on.exit({
print(vis_drake_graph(the.plan,targets_only = TRUE))
})

#Custom cleaning function
clean_regex <- function(regex) {
  clean(list=grep(regex,cached(),value=TRUE))
}

#'========================================================================
# Find out, who's been naughty or nice... 
#'========================================================================
#Set parallelism
options(clustermq.scheduler = "multicore")

#Paw Patrol - så er det nu!
make(the.plan, 
     parallelism = "clustermq", 
     jobs = n.cores,
     keep_going = FALSE,
     verbose=1)

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
