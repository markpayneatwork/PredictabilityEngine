#'========================================================================
# B2.Statmeans
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Mar 26 11:39:37 2021
#
# Calculate means and medians over the statistics generated from each realisation. This can
# give different results when we are dealing with nonlinear transformations. 
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
cat(sprintf("\n%s\n","B2.Statmeans"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

suppressPackageStartupMessages({
  library(PredEng)
})

pcfg <- PE.load.config()

#'========================================================================
# Setup ####
#'========================================================================
#Open database
stats.tbl <- PE.db.tbl(pcfg,PE.cfg$db$stats)

#'========================================================================
# And Go ####
#'========================================================================
#'Work directly from database
#'Base.dat corresponds to the statistics applied to each realisation
base.tbl <- 
  stats.tbl  %>%
  filter(is.na(field),
         month(date) %in% !!pcfg@MOI,
         !resultName %like% "%RollMean%",   #Don't include the rolling means.
         !resultName %like% "%stat_me%",
         !realization %in% c("grandens","realmean","ensmean"))

#'First, the grand ensembles
log_msg("Grand ensemble stat means / medians...\n")
grandens.smms<- 
  base.tbl  %>%
  #Group and summarise
  group_by(srcType,calibrationMethod,startDate,date,
           spName,statName,resultName,.drop=TRUE) %>%
  summarise(stat_median=median(value,na.rm=TRUE),
            stat_mean=mean(value,na.rm=TRUE)) %>%
  collect() %>%
  ungroup() %>%
  #Now polish and massage
  pivot_longer(starts_with("stat_")) %>%
  unite("resultName",c(resultName,name),sep = "/") %>%
  mutate(srcName="grandens",
         realization="grandens")

#Then the individual models
log_msg("Stat means / medians by individual data sources...\n")
realmean.smms<- 
  base.tbl %>%
  #Group and summarise
  group_by(srcType,srcName,calibrationMethod,startDate,date,
           spName,statName,resultName,.drop=TRUE) %>%
  summarise(stat_median=median(value,na.rm=TRUE),
            stat_mean=mean(value,na.rm=TRUE)) %>%
  collect() %>%
  ungroup() %>%
  #Now polish and massage
  pivot_longer(starts_with("stat_")) %>%
  unite("resultName",c(resultName,name),sep = "/") %>%
  mutate(realization="realmean")

#And we can now use this as the input to calculate ensmean metrics as
#as well. Note that we don'y calculate medians in this case, as the 
#number of models is generally small
log_msg("Ensemble mean - Stat means / medians...\n")
ensmean.smms <- 
  realmean.smms %>%
  filter(str_ends(resultName,"stat_mean")) %>%
  #Group and summarise
  group_by(srcType,calibrationMethod,startDate,date,
           spName,statName,resultName,.drop=TRUE) %>%
  summarise(value=mean(value,na.rm=TRUE),
            .groups="drop") %>%
  #Now polish and massage
  mutate(realization="ensmean",
         srcName="ensmean")

#Observation data gets a direct passthrough, so that there is still something to compare against
#for these new resultNames
obs.dat <- 
  stats.tbl  %>%
  filter(is.na(field),
         month(date) %in% !!pcfg@MOI,
         !resultName %like% "%RollMean%",   #Don't include the rolling means.
         !resultName %like% "%stat_me%",
         realization=="realmean" & srcType=="Observations") %>%
  select(-pKey) %>%
  collect()
  
obs.smms <-
  obs.dat %>%
  mutate(resultName=map(resultName,~ paste(.x,c("stat_mean","stat_median"),sep="/"))) %>%
  unnest(resultName)

#'========================================================================
# Complete ####
#'========================================================================
#'#Delete existing results 
log_msg("Getting list of previous ids to clear...")
this.query.time <- 
  system.time({
    del.these.pKeys <-
      stats.tbl %>%
      filter(resultName %like% "%stat_me%") %>%
      pull(pKey)
  })
log_msg("Complete in %0.3fs. \nDeleting...\n",this.query.time[3])
this.query.time <- 
  system.time({
    n <- PE.db.delete.by.pKey(object=pcfg,
                              table=PE.cfg$db$stats,
                              src=NULL,
                              pKeys = del.these.pKeys,
                              silent = TRUE)
  })
log_msg("Deleted %i rows in %0.3fs.\n\n",n,this.query.time[3])

#Write back to table
smms.out <- 
  bind_rows(grandens.smms,
            realmean.smms,
            ensmean.smms,
            obs.smms) 
PE.db.appendTable(pcfg,PE.cfg$db$stats,src=NULL,dat=smms.out)


#Turn off the lights
dbDisconnect(stats.tbl)
if(length(warnings())!=0) print(warnings())
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
