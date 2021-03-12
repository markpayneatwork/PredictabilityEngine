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
  library(RcppRoll)
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

#Clear existing metrics table
if(file.exists(PE.db.path(pcfg,PE.cfg$db$metrics,src=NULL))) {
  file.remove(PE.db.path(pcfg,PE.cfg$db$metrics,src=NULL))
}

#'========================================================================
# Extract  Observations ####
#'========================================================================
log_msg("Extract observations...\n")

#Import uncalibrated observation stats for the target months
#These are the references against which everything else is compared.
obs.dat.all <-
  stats.tbl %>%
  filter(srcType=="Observations",
         is.na(calibrationMethod),# Only uncalibrated observations
         is.na(field)) %>%  #Only values. No fields
  collect() %>%
  #Setup year-month key
  mutate(date=ymd(date),
         ym=date_to_ym(date)) %>%
  filter(month(date) %in% pcfg@MOI) %>%
  #Drop unused fields relating to forecasts
  dplyr::select(-calibrationMethod,-realization,-startDate,-field)

#Simplified versions 
obs.dat <-
  obs.dat.all %>%
  filter(year(date) %in% pcfg@comp.years) 

obs.dat.bare <- #Simplified version for matching with forecasts
  obs.dat %>%
  dplyr::select(spName,statName,resultName,ym,value)

#Some checks
if(nrow(obs.dat.all)!=0) quit(status=0)  #Stop execution
assert_that(length(unique(obs.dat$srcName))==1,msg="Multiple source names detected")
assert_that(all(month(obs.dat$date) %in% pcfg@MOI),
            msg="Mismatch between months in database and MOI.")

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

assert_that(all(!is.na(obs.clim$clim.mean)),
            all(!is.na(obs.clim$clim.sd)),
            msg="NAs detected in climatology")

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
#'
#'Note that the definition of persistence that we use here is persisting the
#'stat forward in time. We first need to work out what data is available on 
#'a given date 
log_msg("Persistence...\n")

#Calculate running averages for persistence forecasts 
#Note that we do this again ourselves, as we want
#the right-aligned window (ie for the previous n years),
#whereas all of the other running averages are based on
#the centered window.
#Also don't want any smoothed data
persis.dat <-
  obs.dat.all %>%
  #Apply smoothing self
  filter(!grepl("/.+$",resultName)) %>%   #Remove smoothed variables
  group_by(srcType,srcName,spName,statName,resultName) %>%
  arrange(date,.by_group=TRUE) %>%
  mutate(RollMean03=roll_meanr(value,n=3,fill=Inf),  #Use Inf (rather than NA) to indicate can't calculate
         RollMean05=roll_meanr(value,n=5,fill=Inf),  #Want to propigate NAs if they exist, as error check
         RollMean07=roll_meanr(value,n=7,fill=Inf),
         RollMean09=roll_meanr(value,n=9,fill=Inf),
         srcType="Persistence") %>%
  ungroup() %>%
  #Pivot longer and merge averages into value column
  rename(rawValue=value)%>%
  pivot_longer(c(rawValue,starts_with("RollMean")),
               names_to = "averaging",values_to = "value") %>%
  unite("resultName",c("resultName","averaging"),sep="/") %>%
  mutate(resultName=gsub("/rawValue$","",resultName)) %>%  #Don't label raw value 
  #Drop persistences where we can't calculate due to rolling window limitations
  filter(!is.infinite(value))
  
#Calculate the persistences that we want to include
persis.grid <- 
  #Setup grid
  expand_grid(date=unique(obs.dat$date),
              lead=pcfg@persistence.leads) %>%
  #Drop non-relevant forecasts
  filter(year(date) %in% pcfg@comp.years) %>%
  mutate(startDate=date-months(lead),
         startDate.ym=date_to_ym(startDate))

#Matching up the available data with the requested startDates
#is done via a lookup table
availability.tbl <-
  expand_grid(request.startDates=unique(persis.grid$startDate),  #Generate combinations
              available.dates=unique(persis.dat$date)) %>%
  mutate(difference=month_diff(request.startDates,available.dates)) %>%  #Calculate difference
  filter(difference>=0) %>%  #Remove all those in the future (but same month is ok)
  group_by(request.startDates) %>%
  filter(difference==min(difference)) %>%  #Retain closest match
  mutate(n=n()) %>%
  ungroup() %>%
  mutate(request.ym=date_to_ym(request.startDates))

assert_that(all(availability.tbl$n==1),
            msg="Failure in generation of lookup table.")

#Combine the grid and the data to generate forecasts
persis.forecasts <-
  #Merge in availability table
  left_join(x=persis.grid,
            y=availability.tbl,
            by=c(startDate.ym="request.ym")) %>%
  dplyr::select(date,lead,request.startDates,available.dates) %>%
  mutate(available.ym=date_to_ym(available.dates)) %>%
  #Join with data
  left_join(y=persis.dat,
            by=c(available.ym="ym"),
            suffix=c("",".availDate")) %>%
  #Tidy up to merge with rest
  transmute(srcType,srcName,
            calibrationMethod=NA_character_,
            realization=NA_character_,
            startDate=request.startDates,
            date=date,
            lead,
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
  filter(srcType != "Observations") %>%
  summarise(n=n()) %>%
  collect()  %>%
  pull()

#If the database hasn't been run with forecast models, then there is nothing to do here
have.mdl.dat <- n.mdl.stats !=0
if(have.mdl.dat) {
  mdl.stats.dat <- 
    #Import relevant data first
    stats.tbl %>%
    filter(srcType != "Observations") %>%
    dplyr::select(-field,-pKey) %>% 
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
              dplyr::select(clim.as.forecast,-starts_with("clim")),
              mdl.stats.dat)

} else { #Only process persistence data
  pred.dat <- persis.stats.dat
  
}

#'========================================================================
# Calculate the metrics for central tendencies ####
#'========================================================================
log_msg("Central tendency metrics...\n")

#Skill functions
cent.skill.fn <- function(d) { 
  #i <- 97
  #d <- cent.pred$data[[i]]

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
    summarise(metric="MSE",
              draws=list(mean)) %>%
    mutate(value=mean(err))
  
  #Correlation coefficent
  resamp.cor <-
    tibble(idxs=map(1:n.samples,~sample(1:nrow(xy),nrow(xy),replace=TRUE)),
           xy=map(idxs,~ xy[.x,]),
           cor=map_dbl(xy,~cor(.x[,1],.x[,2]))) %>%
    filter(!is.na(cor)) %>%
    summarise(metric="pearson.correlation",
              draws=list(cor)) %>%
    mutate(value=cor(xy[,1],xy[,2],use="pairwise.complete.obs"))

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
  #Nest
  group_by(srcType,srcName,calibrationMethod,realization,
         spName,statName,resultName,lead,
         .drop = TRUE) %>%
  group_nest() 

#Dry run
log_msg("Dry run....")
run.time <- system.time({dmp.cal <- cent.skill.fn(cent.pred$data[[1]])})
log_msg("Complete in %0.3fs.\n",run.time[3])
log_msg("Sanity check passed. Parallellising using %i cores...\n",n.cores)

#Now calculate the skill in a parallelised manner
cent.metrics <- 
  cent.pred %>%
  mutate(metrics=future_map(data,
                            cent.skill.fn,
                            .options = furrr_options(stdout=FALSE,
                                                     seed=TRUE))) %>%
  dplyr::select(-data) %>% 
  unnest(metrics)

#Write these results
cent.metrics %>%
  PE.db.appendTable(pcfg,PE.cfg$db$metrics,src=NULL,dat=.)

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
    group_by(srcType,srcName,calibrationMethod,startDate,date,
             spName,statName,resultName,.drop=TRUE) %>%
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
      dplyr::select(-value) %>%
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
             statName,resultName,lead,.drop=TRUE) %>%
    group_nest()
  
  #Skill functions
  dist.skill.fn <- function(d) { 
    #d <- dist.dat$data[[1]]
    
    #Setup
    xy <- 
      d %>%
      dplyr::select(value,pred.mean,pred.sd) %>%
      na.omit() 
    
    #CRPS
    resamp.CRPS <-
      tibble(idxs=map(1:n.samples,~sample(1:nrow(xy),nrow(xy),replace=TRUE)),
             xy=map(idxs,~ xy[.x,]),
             crps=map_dbl(xy,~crps(.x$value,cbind(.x$pred.mean,.x$pred.sd))$CRPS)) %>%
      filter(!is.na(crps)) %>%
      summarise(metric="crps",
                draws=list(crps)) %>%
      mutate(value=crps(xy$value,cbind(xy$pred.mean,xy$pred.sd))$CRPS)
    
    return(resamp.CRPS)
  }
    
  #Now calculate statistics
  dist.metrics <- 
    dist.dat %>%
    mutate(metrics=future_map(data,
                              dist.skill.fn,
                              .options = furrr_options(stdout=FALSE,
                                                       seed=TRUE))) %>%
    dplyr::select(-data) %>% 
    unnest(metrics) %>%
    mutate(realization="realmean")
  
  #Write these results
  dist.metrics %>%
    PE.db.appendTable(pcfg,PE.cfg$db$metrics,src=NULL,dat=.)
  
  
  #'========================================================================
  # Skill scores ####
  #'========================================================================
  log_msg("Skill scores...\n")
  #Calculate skill scores where possible
  #First, extract the observation (climatology)-based metrics
  #Note that we don't permit any error in the climatological metrics
  #This is perhaps an oversight, but it is much simpler.
  clim.metrics <-
    bind_rows(cent.metrics,dist.metrics) %>%
    filter(srcType=="Climatology") %>%
    ungroup()   %>%
    dplyr::select(spName,statName,resultName,metric,value,draws)
  
  #Calculate skill scores by merging back into metrics
  skill.scores <- 
    bind_rows(cent.metrics,dist.metrics) %>%
    filter(metric %in% c("crps","MSE")) %>%
    left_join(y=clim.metrics,
              by=c("spName","statName","resultName","metric"),
              suffix=c(".mdl",".ref")) %>%
    mutate(value=1-value.mdl/value.ref,
           draws=map2(draws.mdl,value.ref, ~ 1-.x/.y),
           draws2=map2(draws.mdl,draws.ref,~1-.x/.y),
           UL=map_dbl(draws,quantile,prob=0.05),
           LL=map_dbl(draws,quantile,prob=0.95),
           UL2=map_dbl(draws2,quantile,prob=0.05),
           LL2=map_dbl(draws2,quantile,prob=0.95),
           IQR.mdl=map_dbl(draws.mdl,IQR),
           IQR.ref=map_dbl(draws.ref,IQR),
           IQR.ratio=IQR.mdl/IQR.ref) %>%
    #Rename skill scores and tidy
    mutate(metric=case_when(metric=="crps" ~"crpss",
                            metric=="MSE" ~ "MSSS",
                            TRUE~NA_character_)) 
  
  #Drop exploratory columns and write to database
  skill.scores %>%
    dplyr::select(all_of(names(dist.metrics))) %>%
    PE.db.appendTable(pcfg,PE.cfg$db$metrics,src=NULL,dat=.)  
}

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
dbDisconnect(stats.tbl)
plan(sequential)
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
