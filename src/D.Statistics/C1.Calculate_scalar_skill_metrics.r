#'========================================================================
# Calculate_skill_metrics
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed May  1 10:48:18 2019
#
# Calculates skill metrics 
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
cat(sprintf("\n%s\n","Calculate scalar skill metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
start.time <- proc.time()[3]; 

#Helper functions, externals and libraries
suppressMessages({
  library(verification)
  library(PredEng)
  library(furrr)
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

#Setup parallelisation
if(Sys.info()["nodename"]=="aqua-cb-mpay18" | interactive()) {
  n.cores <- availableCores()
} else {
  n.cores <- as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))    
  assert_that(!is.na(n.cores),msg = "Cannot detect number of allocated cores")
}
plan(multisession,workers = n.cores)

#'========================================================================
# Setup ####
#'========================================================================
#ASSERTION: there is only one month of interest defined here. We may need to
#extend this in the future
assert_that(length(pcfg@MOI)==1,msg="Metric calculation currently one works with one MOI")

#Setup databases
this.db <- PE.db.connection(pcfg)
stats.tbl <- tbl(this.db,PE.cfg$db$stats)

# #Reset the resutls table by deleting and reestablishing it
# dbRemoveTable(this.db,PE.cfg$db$stats)
# PE.db.setup(pcfg)

#Clear existing metrics table
if(dbExistsTable(PE.db.connection(pcfg),PE.cfg$db$metrics)) {
  dbRemoveTable(PE.db.connection(pcfg),PE.cfg$db$metrics)  
}

#'========================================================================
# Extract  Observations ####
#'========================================================================
log_msg("Extract observations...\n")

#Import observation stats
obs.dat <-
  stats.tbl %>%
  filter(srcType=="Observations") %>% 
  select(-field,-pKey) %>%
  collect() %>%
  #Setup year-month key
  mutate(date=ymd(date),
         ym=date_to_ym(date)) %>%
  #Drop NAs (which are probably field-only)
  filter(!is.na(value)) %>%
  #Restrict to comparison years
  filter(year(date) %in% pcfg@comp.years) %>%
  #Drop unused fields relating to forecasts
  select(-calibrationMethod,-realization,-startDate,-leadIdx)

#Some checks
assert_that(length(unique(obs.dat$srcName))==1,msg="Multiple source names detected")
assert_that(all(month(obs.dat$date) %in% pcfg@MOI),
            msg="Mismatch between months in database and MOI.")

#Simplified version for matching with forecasts
obs.dat.bare <- 
  obs.dat %>%
  select(spName,statName,resultName,ym,value)

#Create a data.frame with the observations and the corresponding mean and standard deviation
#of each stat result over the climatological period. This is used in generating skill
#scores of both the distributional and central tendency metrics
obs.clim <-
  #Calculate statistics
  obs.dat %>%
  filter(year(date) %in% pcfg@clim.years) %>%
  group_by(spName,statName,resultName) %>%
  summarise(clim.mean=mean(value),
            clim.sd=sd(value),
            clim.n=n(),
            .groups="drop")

assert_that(all(obs.clim$clim.n==length(pcfg@clim.years)),
            msg="Mismatch between number of clim years and amount of data")

#Setup forecasts using the climatology as the forecast as well.
clim.as.forecast <- 
  left_join(x=obs.dat,
            y=obs.clim,
            by=c("spName","statName","resultName")) %>%
  mutate(value=clim.mean,
         srcType="Climatology") %>%
  filter(year(date) %in% pcfg@comp.years)

#'========================================================================
# Setup persistence forecasts ####
#'========================================================================
#'At the moment, we only apply persistence forecasts to scalar variables.
#'However, the approach could (quite) easily be applied over to fields as
#'well, especially given that we now are applying the meanadjusted calibration
#'method to the observations as well (especially giving us the basis for an 
#'anomaly persistence forecast)

log_msg("Persistence...\n")

persis.grid <- 
  #Setup grid
  expand_grid(date=unique(obs.dat$date),
              lead=pcfg@persistence.leads) %>%
  #Drop non-relevant forecasts
  filter(year(date) %in% pcfg@comp.years) %>%
  mutate(startDate=date-months(lead),
         startDate.ym=date_to_ym(startDate))

persis.dat <-
  stats.tbl %>%
  filter(srcType=="Persistence",
         !is.na(value)) %>%
  select(-field) %>%
  mutate(startDate.ym=str_sub(startDate,1,7)) %>%
  filter(startDate.ym %in% !!persis.grid$startDate.ym) %>%
  collect() %>%
  select(-pKey,-realization,-date,-leadIdx,-startDate)

persis.forecasts <-
  left_join(x=persis.grid,
            y=persis.dat,
            by=c("startDate.ym")) %>%
  #Tidy up to merge with rest
  transmute(srcType,srcName,
            calibrationMethod,
            realization=NA_character_,
            startDate,date,
            spName,statName,resultName,value,
            ym=date_to_ym(date))

#'========================================================================
# Extract other data sources ####
#'========================================================================
#Now merge observations with stats from the other data sources We note
#that we do the merging by yearmonth - this should generally be ok for most of the
#situations where we envisage using PredEnd i.e. one data point per year - but
#we need to be aware that this is not exactly the case 
log_msg("Model data...\n")
#Check whether we have any data other than observations in the database
n.mdl.stats <- 
  stats.tbl %>%
  filter(!srcType %in% c("Observations","Persistence")) %>%
  summarise(n=n()) %>%
  collect()  %>%
  pull()

#If the database hasn't been run with forecast models, then there is nothing to do here
have.mdl.dat <- n.mdl.stats !=0
if(have.mdl.dat) {
  mdl.stats.dat <- 
    #Import relevant data first
    stats.tbl %>%
    filter(!srcType %in% c("Observations","Persistence")) %>%
    select(-field,-pKey,-leadIdx) %>% 
    filter(!is.na(value)) %>%
    collect() %>%
    #Tweak
    mutate(startDate=ymd(startDate),
           date=ymd(date),
           ym=date_to_ym(date)) %>%
    #Comparison years only
    filter(year(date) %in% pcfg@comp.years)
  
  #Now combine with persistence forecasts
  pred.dat <- 
    bind_rows(persis.forecasts,
              select(clim.as.forecast,-starts_with("clim")),
              mdl.stats.dat)

} else { #Only process persistence data
  pred.dat <- persis.stats.dat
  
}

#'========================================================================
# Calculate the metrics for central tendencies ####
#'========================================================================
log_msg("Central tendency metrics...\n")

#Skill functions
skill.fn <- function(d,n.samples=1000,probs=c(0.025,0.5,0.975)) { 

  #Setup
  xy <- 
    cbind(d$value.obs,d$value.pred) %>%
    na.omit() 
  
  err <-
    (xy[,1]-xy[,2]) ^2

  #RSE
  resamp.mse <- 
    tibble(samples=map(1:n.samples,~sample(err,length(err),replace=TRUE)),
           mean=map_dbl(samples,mean)) %>%
    summarise(CI=probs,
              value=quantile(mean,prob=probs,names=FALSE)) %>%
    bind_rows(tibble(CI=NA,value=mean(err))) %>%
    mutate(metric="MSE")
  
  #Correlation coefficent
  resamp.cor <-
    tibble(idxs=map(1:n.samples,~sample(1:nrow(xy),nrow(xy),replace=TRUE)),
           xy=map(idxs,~ xy[.x,]),
           cor=map_dbl(xy,~cor(.x[,1],.x[,2]))) %>%
    filter(!is.na(cor)) %>%
    summarise(CI=probs,
              value=quantile(cor,prob=probs,names=FALSE)) %>%
    bind_rows(tibble(CI=NA,value=cor(xy[,1],xy[,2]))) %>%
    mutate(metric="pearson.correlation")

  bind_rows(resamp.mse,resamp.cor)
}

#First setup the forecast data
cent.pred <- 
  pred.dat %>%
  #Only want persistence, realmean or ensmean
  filter(realization %in% c("realmean","ensmean") | 
           srcType %in% c("Persistence","Climatology")) %>%
  #Merge in the observations
  left_join(y=obs.dat.bare,
            by=c("ym","spName","statName","resultName"),
            suffix=c(".pred",".obs")) %>%
  #Add lead
  mutate(lead=month_diff(date,startDate))  %>%
  #Nest
  group_by(srcType,srcName,calibrationMethod,realization,
         spName,statName,resultName,lead,
         .drop = TRUE) %>%
  group_nest() 

#Dry run
log_msg("Dry run....")
run.time <- system.time({dmp.cal <- skill.fn(cent.pred$data[[1]])})
log_msg("Complete in %0.3fs.\n",run.time[3])
log_msg("Sanity check passed. Parallellising using %i cores...\n",n.cores)

#Now calculate the skill in a parallelised manner
cent.metrics <- 
  cent.pred %>%
  mutate(metrics=future_map(data,skill.fn)) %>%
  select(-data) %>% 
  unnest(metrics)

#Write these results
cent.metrics %>%
  PE.db.appendTable(pcfg,PE.cfg$db$metrics)

#'========================================================================
# Distribution metrics ####
#'========================================================================
if(have.mdl.dat) {
  log_msg("Distributional metrics...\n")
  # Metrics of the probabilistic forecast distributions
  # The crps requires that the forecasts be expressed in terms of a probability
  # distribution, ie the mean and standard deviation. This requires calculation
  # of these quantities first
  dist.pred <- 
    #Convert to probabilistic forecasts
    mdl.stats.dat %>%  #Don't include persistence
    filter(!(realization %in% c("realmean","ensmean"))) %>%   #Individual ens members only
    group_by(srcType,srcName,calibrationMethod,startDate,date,spName,statName,resultName,.drop=TRUE) %>%
    summarise(pred.mean=mean(value),
              pred.sd=sd(value),
              pred.n=n(),
              .groups="drop") 
  
  #Merge with observations and climatalogy
  dist.dat <-
    #Use climatology as a forecast
    clim.as.forecast %>%
      rename(pred.mean=clim.mean,
             pred.sd=clim.sd,
             pred.n=clim.n) %>%
      select(-value) %>%
    #Add in predictions
    bind_rows(dist.pred) %>%
    #Add in observations
    mutate(ym=date_to_ym(date)) %>%
    left_join(y=obs.dat.bare,
              by=c("ym","spName","statName","resultName"),
              suffix=c(".mdl",".obs")) %>%
    #Tidy
    mutate(lead=month_diff(date,startDate)) %>%
    group_by(srcType,srcName,calibrationMethod,spName,
             statName,resultName,lead,.drop=TRUE)
    
  #Now calculate statistics
  dist.metrics <- 
    dist.dat %>%
    summarise(crps=crps(value,cbind(pred.mean,pred.sd))$CRPS,
              .groups="keep") %>%
    pivot_longer(-group_vars(.),names_to = "metric") %>%
    ungroup() %>%
    mutate(realization="realmean",
           CI=NA)
  
  #'========================================================================
  # Skill scores ####
  #'========================================================================
  log_msg("Skill scores...\n")
  #Calculate skill scores where possible
  #First, extract the observation (climatology)-based metrics
  clim.metrics <-
    bind_rows(cent.metrics,dist.metrics) %>%
    filter(srcType=="Climatology",
           is.na(CI)) %>%
    ungroup()   %>%
    select(spName,statName,resultName,metric,value)
  
  #Calculate skill scores by merging back into metrics
  skill.scores <- 
    bind_rows(cent.metrics,dist.metrics) %>%
    filter(metric %in% c("crps","MSE")) %>%
    left_join(y=clim.metrics,
              by=c("spName","statName","resultName","metric"),
              suffix=c(".mdl",".ref")) %>%
    mutate(value=1-value.mdl/value.ref) %>%
    #Rename skill scores and tidy
    mutate(metric=case_when(metric=="crps" ~"crpss",
                            metric=="MSE" ~ "MSSS",
                            TRUE~NA_character_)) %>%
    select(-value.mdl,-value.ref)
  
  #'========================================================================
  # Complete ####
  #'========================================================================
  log_msg("Output...\n")
  #Now write to database
  met.out <-
    bind_rows(dist.metrics,skill.scores)

  PE.db.appendTable(met.out,pcfg,PE.cfg$db$metrics)
}
#Turn off the lights
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
