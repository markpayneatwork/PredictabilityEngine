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
                         "EN4"="C2.EN4_extraction.r",
                         "HadISST"="C1.HadISST_data.r",
                         stop("Cannot find observation script"))
    callr::rscript(here("src/B.Extract/",obs.script),
                   stdout=log.file("B.Observations.%s",pcfg@Observations@name),
                   stderr=log.file("B.Observations.%s",pcfg@Observations@name))
    script.complete()
  })
}

extract.source.list <- function(this.src) {
  ignore({
    tibble(this.datasrc=if(is(this.src,"PElst")) {this.src@.Data} else {list(this.src)})  %>%
      mutate(srcName=map_chr(this.datasrc,slot,"name"),
             srcType=map_chr(this.datasrc,slot,"type"),
             this.sources=map(this.datasrc,slot,"sources")) %>%
      unnest(this.sources)
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
      callr::rscript(here("src/D.Statistics/A1.Partition_stats.r"))
      return(readRDS(PE.scratch.path(pcfg,"statjoblist")))
    })
}

process.stat <- function(this.stat) {
  ignore({
    #This is not a very elegant solution, but it is the best I can think of for
    #the time being. We want to maintain Calculate_stats as a script, because it
    #make it easy to write out the log files. However that presents challenges with
    #taking full advantage of the hashing feature of drake. We therefore take in the
    #full configuration line, and then convert it to a line id.
    all.jobs <- readRDS(PE.scratch.path(pcfg,"statjoblist"))
    row.match <- 
      all.jobs %>%
      split(f=seq(nrow(.))) %>%
      map_lgl(.,~ isTRUE(all.equal(.x,this.stat)))
    assert_that(sum(row.match)==1,msg="Cannot identify stat job")
    this.id <- which(row.match)
    #Now call the script
    callr::rscript(here("src/D.Statistics/B1.Calculate_stats.r"),
                   cmdargs=this.id,
                   stdout=log.file("D.Stats.%03i",this.id),
                   stderr=log.file("D.Stats.%03i",this.id))
    return(this.stat)
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

#'========================================================================
# The Plan! ####
#'========================================================================
#Debugging
# extract.decadal <- function(...) {script.complete()}
# extract.observations <- function(...) {script.complete()}
# process.stat <- function(...) {script.complete()}
#calc.realmeans <- function(...) {return(list(...))}

#Make a plan
dec.plan <- 
  drake_plan(DecSources=target(extract.source.list(this.src=decadalDatSrc),
                               transform = map(decadalDatSrc=!!pcfg@Decadal@.Data,
                                               .names=paste0("DecSources_",!!names(pcfg@Decadal))),
                               trigger=trigger(command=FALSE)),
             DecExtr=target(extract.decadal(DecSources),
                            transform=map(DecSources),
                            dynamic=map(DecSources),
                            trigger=trigger(command=FALSE)),
             combRealmeans=target(c(DecExtr),  #Simple placeholder
                                  transform=combine(DecExtr),
                                  trigger=trigger(command=FALSE)),
             Realmeans=target(calc.realmeans(this.datasrc=datasrc,combRealmeans),
                              transform=map(datasrc=!!pcfg@Decadal@.Data,
                                            .names=paste0("Realmeans_",!!names(pcfg@Decadal)))),
             DecFrags=target(c(Realmeans),
                             transform=combine(Realmeans)))
#vis_drake_graph(dec.plan,targets_only = TRUE)             

end.game <-
  drake_plan(obs.obj=ignore(pcfg@Observations),
             Observations=target(command=extract.observations(obs.obj),
                                 trigger=trigger(command=FALSE)),
             Extractions=target(list(Observations,DecFrags),
                                trigger=trigger(command=FALSE)),
             Calibration=target(calibration.scripts(Extractions),
                                trigger=trigger(command=FALSE)),
             stat.objs=ignore(pcfg@statistics),
             StatJobs=target(stat.jobs(Calibration,stat.objs),
                             trigger=trigger(command=FALSE)),
             Stats=target(process.stat(StatJobs),
                          dynamic=map(StatJobs),
                          trigger=trigger(command=FALSE)),
             Metrics=target(process.metrics(Stats)))

the.plan <- bind_plans(dec.plan,end.game)


#'========================================================================
# Supplementary ####
#'========================================================================
#Check it twice.
print(vis_drake_graph(the.plan))

#Custom cleaning function
clean_regex <- function(regex) {
  clean(list=grep(regex,cached(),value=TRUE))
}

#'========================================================================
# And Go ####
#'========================================================================
#Set parallelism
options(clustermq.scheduler = "multicore")

#Find out, who's been naughty or nice... Paw Patrol - så er det nu!
make(the.plan, 
     parallelism = "clustermq", 
     jobs = 8,
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
