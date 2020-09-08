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
library(PredEng)
pcfg <- readRDS(PE.cfg$path$config)

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
#Setup databases
this.db <- PE.db.connection(pcfg)
stats.tbl <- tbl(this.db,PE.cfg$db$stats)

# #Reset the resutls table by deleting and reestablishing it
# dbRemoveTable(this.db,PE.cfg$db$stats)
# PE.db.setup(pcfg)

date_to_ym <- function(d) {
  sprintf("%i-%02i",year(d),month(d))
}

#Clear existing metrics table
if(dbExistsTable(PE.db.connection(pcfg),PE.cfg$db$metrics)) {
  dbRemoveTable(PE.db.connection(pcfg),PE.cfg$db$metrics)  
}

#'========================================================================
# Merge In  Observations ####
#'========================================================================
log_msg("Merging...\n")

#Import stats
obs.stats <-
  stats.tbl %>%
  filter(srcType=="Observations") %>% 
  select(-field) %>%
  collect() %>%
  #Setup year-month key
  mutate(ym=date_to_ym(date)) %>%
  select(ym,spName,statName,value) %>%
  #Drop NAs (which are probably field-only)
  filter(!is.na(value))

#And merge it back into the comparison dataframe. This way we have both the
#modelled and the observed results together in the same dataframe. We note
#that we do the merging by yearmonth - this should generally be ok for most of the
#situations where we envisage using PredEnd i.e. one data point per year - but
#we need to be aware that this is not exactly the case 

#To reduce the computational load, we first get a list of values that we actually want to include
comp.these <- 
  stats.tbl %>%
  filter(srcType != "Observations") %>%
  select(pKey,date) %>%
  collect() %>%
  filter(year(date) %in% pcfg@comp.years) %>%
  pull(pKey)

comp.dat <- 
  #Import relevant data first
  stats.tbl %>%
  filter(pKey %in% comp.these) %>%
  select(-field,-pKey) %>% 
  collect() %>%
  mutate(ym=date_to_ym(date)) %>%
  #Remove NAs
  filter(!is.na(value)) %>%
  #Join in observational data
  left_join(obs.stats,
            by=c("ym","statName","spName"),
            suffix=c(".mdl",".obs")) 

#'========================================================================
# Calculate the metrics for scalar statistics ####
#'========================================================================
#Looking to calculate a set of metrics here. In particular, we want to have
#the mean skill, but also the range across initialisation dates as well

#Skill functions
RMSE <- function(x,y) { sqrt(mean((x-y)^2,na.rm=TRUE))}

#Now calculate the skill over all start dates.
#ASSERTION: there is only one month of interest defined here. We may need to
#extend this in the future
g.vars <- c("srcType","srcName","realization","calibrationMethod","spName","statName","leadIdx")
skill.wide <- 
  comp.dat %>%
  group_by(across(all_of(g.vars)),.drop = TRUE) %>%
  summarise(pearson.correlation=cor(value.mdl,value.obs,use="pairwise.complete"),
            RMSE=RMSE(value.mdl,value.obs),
            .groups="keep")  

#Now write to database
skill.wide %>% 
  pivot_longer(-all_of(g.vars),names_to = "metric") %>%
  ungroup() %>%
  PE.db.appendTable(pcfg,PE.cfg$db$metrics)

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
