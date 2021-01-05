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
# Merge In  Observations ####
#'========================================================================
log_msg("Merging...\n")

#Import stats
obs.dat <-
  stats.tbl %>%
  filter(srcType=="Observations") %>% 
  select(srcType,srcName,date,spName,statName,resultName,value) %>%
  collect() %>%
  #Setup year-month key
  mutate(date=ymd(date),
         ym=date_to_ym(date)) %>%
  #Drop NAs (which are probably field-only)
  filter(!is.na(value))

#Simplified version for matching with forecasts
obs.dat.bare <- 
  obs.dat %>%
  select(-srcType,-srcName,-date)

#Create a data.frame with the observations and the corresponding mean and standard deviation
#of each stat result over the entire comparison period. This is used in generating skill
#scores of both the distributional and central tendency metrics
obs.dat.stats <-
  #Calculate statistics
  obs.dat %>%
  filter(year(date) %in% pcfg@comp.years,
         month(date) %in% pcfg@MOI) %>%
  group_by(srcType,srcName,spName,statName,resultName) %>%
  summarise(mean=mean(value),
            sd=sd(value),
            .groups="drop") %>%
  #Join back in 
  left_join(x=obs.dat,by=c("srcType","srcName","spName","statName","resultName")) %>%
  #Filter again to comparison years
  filter(year(date) %in% pcfg@comp.years,
         month(date) %in% pcfg@MOI) %>%
  select(-ym)

#'========================================================================
# Setup persistence forecasts ####
#'========================================================================
#'At the moment, we only apply persistence forecasts to scalar variables.
#'However, the approach could (quite) easily be applied over to fields as
#'well, especially given that we now are applying the meanadjusted calibration
#'method to the observations as well (especially giving us the basis for an 
#'anomaly persistence forecast)

persis.stats.dat <- 
  #Setup grid
  expand_grid(startDate=unique(obs.dat$date),
              lead=pcfg@persistence.leads) %>%
  mutate(forecastDate=startDate+months(lead)) %>%
  #Drop non-relevant forecasts
  filter(month(forecastDate) %in% pcfg@MOI,
         year(forecastDate) %in% pcfg@comp.years) %>%
  #Make the forecasts 
  mutate(startDate=date_to_ym(startDate)) %>%
  left_join(y=obs.dat,
            by=c(startDate="ym")) %>%
  #Tidy up to merge with rest
  transmute(srcType="Persistence",
            srcName,
            calibrationMethod=NA,
            realization=NA,
            startDate=date,
            date=forecastDate,
            spName,statName,resultName,value,
            ym=date_to_ym(date))

#'========================================================================
# Extract other data sources ####
#'========================================================================
#Now merge observations with stats from the other data sources We note
#that we do the merging by yearmonth - this should generally be ok for most of the
#situations where we envisage using PredEnd i.e. one data point per year - but
#we need to be aware that this is not exactly the case 

#To reduce the computational load, we first get a list of values that we actually want to include
mdl.stats.df<- 
  stats.tbl %>%
  filter(srcType != "Observations") %>%
  select(pKey,date) %>%
  collect() %>%
  filter(year(date) %in% pcfg@comp.years) 

#If the database hasn't been run with forecast models, then there is nothing to do here
if(nrow(mdl.stats.df)!=0) {
  mdl.stats <-
    mdl.stats.df %>%
    pull(pKey)

  mdl.stats.dat <- 
    #Import relevant data first
    stats.tbl %>%
    filter(pKey %in% mdl.stats) %>%
    select(-field,-pKey,-leadIdx) %>% 
    collect() %>%
    #Remove NAs
    filter(!is.na(value)) %>%
    #Tweak
    mutate(startDate=ymd(startDate),
           date=ymd(date),
           ym=date_to_ym(date))
  
  #Now combine with persistence forecasts
  pred.dat <- 
    bind_rows(persis.stats.dat,
              mdl.stats.dat)

} else { #Only process persistence data
  pred.dat <- persis.stats.dat
  
}

#'========================================================================
# Calculate the metrics for central tendencies ####
#'========================================================================
#First setup the forecast data
mean.pred <- 
  pred.dat %>%
  #Merge in the observations
  left_join(y=obs.dat.bare,
            by=c("ym","spName","statName","resultName"),
            suffix=c(".pred",".obs")) %>%
  #Tidy up
  select(-ym)

#Skill functions
MSE <- function(x,y) { mean((x-y)^2,na.rm=TRUE)}
RMSE <- function(x,y) { sqrt(MSE(x,y,))}

#Now calculate the skill over all start dates.
g.vars <- c("srcType","srcName","realization","calibrationMethod",
            "spName","statName","resultName","lead")
mean.metrics <- 
  #Customise observation metrics first
  obs.dat.stats %>%
  rename(value.pred=mean,  #Predicted by a reference model using climatology
         value.obs=value) %>%
  select(-sd) %>%
  bind_rows(mean.pred) %>%
  #Add lead
  mutate(lead=month_diff(date,startDate))  %>%
  #Group and calculate metrics
  group_by(across(all_of(g.vars)),.drop = TRUE) %>%
  summarise(pearson.correlation=cor(value.pred,value.obs,use="pairwise.complete"),
            MSE=MSE(value.pred,value.obs),
            n=n(),
            .groups="keep")  %>%
  pivot_longer(-group_vars(.),names_to = "metric") %>%
  ungroup()

#'========================================================================
# Distribution metrics ####
#'========================================================================
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
            .groups="drop") %>%
  #Merge in observations
  mutate(ym=date_to_ym(date)) %>%
  left_join(y=obs.dat.bare,
            by=c("ym","spName","statName","resultName"),
            suffix=c(".mdl",".obs")) %>%
  select(-ym)

#Now calculate statistics
dist.metrics <- 
  #Customise observation metrics first
  obs.dat.stats %>%
  rename(pred.mean=mean,pred.sd=sd) %>%
  bind_rows(dist.pred) %>%
  #Add lead
  mutate(lead=month_diff(date,startDate))  %>%
  #Group and calculate
  group_by(srcType,srcName,calibrationMethod,spName,statName,resultName,lead,.drop=TRUE) %>%
  summarise(crps=crps(value,cbind(pred.mean,pred.sd))$CRPS,
            .groups="keep") %>%
  pivot_longer(-group_vars(.),names_to = "metric") %>%
  ungroup() %>%
  mutate(realization="realmean")

#'========================================================================
# Skill scores ####
#'========================================================================
#Calculate skill scores where possible
#First, extract the observation (climatology)-based metrics
clim.metrics <-
  bind_rows(mean.metrics,dist.metrics) %>%
  filter(srcType=="Observations") %>%
  select(-srcType,-srcName,-calibrationMethod,-lead,-realization)

#Calculate skill scores by merging back into metrics
skill.scores <- 
  bind_rows(mean.metrics,dist.metrics) %>%
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
#Now write to database
met.out <-
  bind_rows(mean.metrics,dist.metrics,skill.scores)  %>%
  filter(srcType!="Observations")

PE.db.appendTable(met.out,pcfg,PE.cfg$db$metrics)

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
