#'========================================================================
# Calculate_field_skill_metrics
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed May  1 10:48:18 2019
#
# Calculates skill metrics for fields
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
cat(sprintf("\n%s\n","Calculate field skill metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
start.time <- proc.time()[3]; 

#Helper functions, externals and libraries
suppressMessages({
  #library(verification)
  library(PredEng)
})
pcfg <- PE.load.config()

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  debug.mode <- TRUE
  set.log_msg.silent()
} else {
  debug.mode <- FALSE
}

#Scale back number of samples to increase debugging speed
if(pcfg@project.name=="TestSuite") {
  n.samples <- 10
} else {
  n.samples <- 1000
}

#'========================================================================
# Setup ####
#'========================================================================
#ASSERTION: there is only one month of interest defined here. We may need to
#extend this in the future
assert_that(length(pcfg@MOI)==1,msg="Metric calculation currently one works with one MOI")

#Setup databases
stats.tbl <- PE.db.tbl(pcfg,PE.cfg$db$stats,src=NULL)

#Reference datasource
ref.datasrc <-  data.source(type="Observations",name=pcfg@reference)  

#Clear existing metrics table
if(file.exists(PE.db.path(pcfg,PE.cfg$db$metrics.field,src=NULL))) {
  file.remove(PE.db.path(pcfg,PE.cfg$db$metrics.field,src=NULL))
}

#'========================================================================
# Merge In  Observations ####
#'========================================================================
log_msg("Merging...\n")

#Import uncalibrated observations that have field data with them
ref.dat <-
  stats.tbl %>%
  filter(srcType==!!ref.datasrc@type,
         srcName==!!ref.datasrc@name,
         calibrationMethod=="None",# Only uncalibrated observations
         !is.na(field)) %>% 
  select(srcType,srcName,date,spName,statName,resultName,field) %>%
  collect() %>%
  PE.db.unserialize() %>%
  #Setup year-month key
  mutate(date=ymd(date),
         ym=date_to_ym(date)) %>%
  filter(month(date) %in% pcfg@MOI) 

#Strip back to bare bones
ref.dat.bare <-
  ref.dat %>%
  select(spName,statName,resultName,ym,field)

#'========================================================================
# Skill Functions ####
#'========================================================================
#Correlation coefficent, with resampling
skill.fn <- function(ref,pred,do.draws) {
  # ref <- this.dat$field.ref
  # pred <- this.dat$field.pred
  
  #Setup
  ref.b <- brick(ref)
  pred.b <- brick(pred)
  assert_that(nlayers(ref.b)==nlayers(pred.b),
              msg="Mismatch in number of layers")
  
  #Calculate metrics
  draw.idxs <- map(1:n.samples,~sample(1:nlayers(ref.b),nlayers(ref.b),replace=TRUE))
  rtn <- list()
  rtn$pearson.correlation <- corLocal(ref.b,pred.b,method="pearson",ngb=1)
  rtn$MSE <- mean((ref.b-pred.b)^2)
  if(do.draws) {
    rtn$pearson.correlation.draws <-
      map(draw.idxs,
          ~corLocal(ref.b[[.x]],
                    pred.b[[.x]],
                    method="pearson",
                    ngb=1)) %>%
      brick()
    rtn$MSE.draws <-
      map(draw.idxs,
          ~mean((ref.b[[.x]]-pred.b[[.x]])^2)) %>%
      brick()
  }
  rtn %>%
    enframe(name = "metric",value="field") %>%
    pivot_wider(names_from="metric",values_from="field") %>%
    return()
}

#'========================================================================
# Loop over data sources ####
#'========================================================================
#To lower the computational load, we don't load all of the data into
#memory at once, but take it in chunks
log_msg("Processing in chunks...\n")

#First get an overview of what's available and what we actually want to
#process
mdl.meta <-
  stats.tbl %>%
  filter(srcType!="Observations",
         realization %in% c("ensmean","grandens","realmean"),
         !is.na(field)) %>% #Only want the fields
  select(-field,-value) %>%
  collect() %>%
  #Restrict to comparison years
  mutate(date=ymd(date)) %>%
  filter(year(date) %in% pcfg@comp.years) %>%
  #Group into chunks for further processing
  select(pKey,srcType,srcName,lead,spName,statName,resultName) %>%
  group_by(across(-pKey),.drop=TRUE) %>%
  nest(pKeys=c(pKey)) %>%
  mutate(pKeys=map(pKeys,pull)) %>%
  ungroup()

#Loop over pKey lists
n.groups <- nrow(mdl.meta)
pb <- PE.progress(n.groups)
dmp <- pb$tick(0)
met.l <- list()
for(i in seq(n.groups)) {
  #Setup data
  this.dat <- 
    stats.tbl %>%
    filter(pKey %in% !!mdl.meta$pKeys[[i]]) %>% 
    select(-pKey,-value) %>%
    collect() %>%
    PE.db.unserialize() %>%
    #Setup year-month key
    mutate(date=ymd(date),
           ym=date_to_ym(date),
           lead=month_diff(date,startDate))  %>%
    #Merge in observations
    left_join(y=ref.dat.bare,
              by=c("ym","spName","statName","resultName"),
              suffix=c(".pred",".ref")) %>%
    #Sort
    arrange(date)
  
  #Calculate metrics
  this.met <-
    this.dat %>%
    nest(data=-c(srcType,srcName,calibrationMethod,realization,lead,
                 spName,statName,resultName)) %>%
    #Only do redraws for some data types
    mutate(do.draws=realization=="grandens") %>%
    #Calculate
    mutate(metrics=map2(data,do.draws,~skill.fn(.x$field.ref,.x$field.pred,do.draws=.y))) %>%
    dplyr::select(-data,-do.draws) %>% 
    unnest(metrics)
  
  #Store results
  met.l[[i]] <- this.met

  #Loop
  pb$tick()
  
}

#Bind
mdl.mets <-
  bind_rows(met.l)

#'========================================================================
# Skill Score ####
#'========================================================================
#Calculate observed stat climatologies first
ref.stat.clim <- 
  ref.dat %>%
  filter(year(date) %in% pcfg@clim.years,
         month(date) %in% pcfg@MOI) %>%
  group_by(srcType,srcName,spName,statName,resultName) %>%
  summarise(clim=list(mean(brick(field))),
            .groups="drop")

#Calculate reference (climatology) metrics
ref.clim.mets <-
  ref.dat %>%
  filter(year(date) %in% pcfg@comp.years,
         month(date) %in% pcfg@MOI)%>%
  left_join(y=ref.stat.clim,
            by=c("srcType","srcName","spName","statName","resultName")) %>%
  #Calculate climatology metrics
  group_by(srcType,srcName,spName,statName,resultName) %>%
  summarise(MSE.clim=list(mean((brick(field)-brick(clim))^2)),
            .groups="drop")

#Now calculate skill scores
SS.dat <- 
  mdl.mets %>%
  left_join(y=select(ref.clim.mets,-srcType,-srcName),
            by=c("spName","statName","resultName")) %>%
  mutate(MSSS=map2(MSE,MSE.clim,~ 1- .x/.y),
         MSSS.draws=map2(MSE.draws,MSE.clim, ~ if(is.null(.x)) { NULL} else {1- .x/.y}))

#'========================================================================
# Complete ####
#'========================================================================
#Now write to database
these.mets <- 
  SS.dat %>%
  select(-MSE.clim) %>%
  pivot_longer(-c(srcType,srcName,calibrationMethod,realization,lead,spName,statName,resultName),
               names_to = "metric",values_to = "field") %>%
  ungroup() %>%
  #Drop nulls
  mutate(field.is.null=map_lgl(field,is.null)) %>%
  filter(!field.is.null) %>%
  select(-field.is.null) %>%
  #Make sure everything is in memory before writing
  mutate(field=map_if(field,~!inMemory(.x),~readAll(.x)))

PE.db.appendTable(pcfg,PE.cfg$db$metrics.field,src=NULL,dat=these.mets)

#Turn off the lights
dbDisconnect(stats.tbl)
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
