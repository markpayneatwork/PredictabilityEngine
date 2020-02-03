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
# Calculates skill metrics based on a supplied database of scalar statistics
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
library(tidyverse)
library(PredEng)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configure ####
#'========================================================================

#'========================================================================
# Setup ####
#'========================================================================
#Setup Directories
base.dir <- pcfg@scratch.dir

#Get meta data on the different types of stats that we want
stat.slot <- function(x) {sapply(pcfg@statistics,slot,name=x)}
stats.meta <- slotNames("stat") %>%
  sapply(stat.slot,simplify=FALSE) %>%
  c(list(returns.field=sapply(pcfg@statistics,returns.field))) %>%
  bind_rows()

#Import scalar results
all.scalars <- readRDS(file.path(base.dir,PE.cfg$files$scalar.stats))

#'========================================================================
# Split and Merge Observations ####
#'========================================================================
log_msg("Split and merge...\n")

#Extract out the observational data
obs.dat <- 
  filter(all.scalars,src.type=="Observations") %>%
  dplyr::select(sp.subdomain,ym,stat.name,value) 

#And merge it back into the comparison dataframe. This way we have both the
#modelled and the observed results together in the same dataframe. We note
#that we do the merging by yearmonth - this should generally be ok for most of the
#situations where we envisage using PredEnd i.e. one data point per year - but
#we need to be aware that this is not exactly the case 
comp.dat <- 
  #Remove observation data first
  all.scalars %>%
  filter(src.type!="Observations") %>%
  #Join in observational data
  left_join(obs.dat,
            by=c("ym","stat.name","sp.subdomain"),
            suffix=c(".mdl",".obs")) %>%
  #Restrict to the comparison years 
  filter(year(date) %in% pcfg@comp.years) 
  
  
#Calculate lead time manually
date.to.months <- function(x) {
  #Months since 1 Jan 1900
  (year(x)-1900)*12 + (month(x)-1) + (day(x)-1)/days_in_month(x)
}

#Do lead calculations here 
comp.dat <-
  comp.dat %>%
  mutate(start.month=month(start.date),
         target.month=month(date),
         lead=floor(date.to.months(date)- date.to.months(start.date)))


#'========================================================================
# Calculate the metrics for scalar statistics ####
#'========================================================================
#Looking to calculate a set of metrics here. In particular, we want to have
#the mean skill, but also the range across initialisation dates as well

log_msg("Metrics for scalar statistics...\n")

#Skill functions
RMSE <- function(x,y) { sqrt(mean((x-y)^2,na.rm=TRUE))}
skill.sum <- function(d) {
  d %>%   summarize(cor=cor(value.mdl,value.obs,use="pairwise.complete"),
                    RMSE=RMSE(value.mdl,value.obs))
}

#Now calculate the mean skill over all start dates
g.vars <- c("src.name","src.type","sp.subdomain","stat.name","lead")
skill.mean <- 
  comp.dat %>%
  group_by_at(vars(one_of(g.vars))) %>%
  skill.sum() %>%
  gather("skill.metric","value",-one_of(g.vars)) %>%
  mutate(skill.type="mean.skill")

#Now calculate the range of skill over start dates
skill.starts <- 
  comp.dat %>%
  group_by_at(vars(one_of(c(g.vars,"start.month")))) %>%
  skill.sum()%>%  
  gather("skill.metric","value",-one_of(c(g.vars,"start.month"))) %>%
  ungroup() %>%
  filter(!is.infinite(value)) %>%
  mutate(skill.type=sprintf("start.%02i",start.month)) %>%
  dplyr::select(-start.month)

#Now calculate the range of skill over start dates
skill.range <- 
  skill.starts %>%
  group_by_at(vars(one_of(c(g.vars,"skill.metric")))) %>%
  summarize(max.skill=max(value,na.rm=TRUE),
            median.skill=median(value,na.rm=TRUE),
            min.skill=min(value,na.rm=TRUE)) %>%
  gather("skill.type","value",ends_with("skill"))

#Merge into one tibble 
skill.mets <- 
  bind_rows(skill.range,skill.starts,skill.mean) %>%
  ungroup()
saveRDS(skill.mets,file=file.path(base.dir,PE.cfg$files$scalar.skill.metrics))

#' #'========================================================================
#' # Calculate the metrics for field statistics ####
#' #'========================================================================
#' log.msg("Metrics for field statistics...\n")
#' fldstats <- filter(stats.meta,returns.field)
#' fldstats.dat <- filter(comp.dat,stat.name %in% fldstats$name)
#' 
#' cor.field.skill.fn <- function(mdl.l,obs.l){
#'   mdl.b <- brick(mdl.l)
#'   obs.b <- brick(obs.l)
#'   return(list(corLocal(mdl.b,obs.b)))
#' } 
#' 
#' RMSE.field.skill.fn <- function(mdl.l,obs.l){
#'   mdl.b <- brick(mdl.l)
#'   obs.b <- brick(obs.l)
#'   err.b <- mdl.b - obs.b
#'   rmse.b <- sqrt(mean(err.b^2))
#'   return(list(rmse.b))
#' } 
#' 
#' field.skill <- d <- fldstats.dat %>%
#'   group_by_at(vars(one_of(g.vars))) %>%
#'   summarize(cor=cor.field.skill.fn(field.mdl,field.obs),
#'             RMSE=RMSE.field.skill.fn(field.mdl,field.obs)) %>%
#'   gather("skill.metric","field.skill",-one_of(g.vars)) %>%
#'   mutate(skill.type="field")
#' 
#' comb.skill <- bind_rows(skill.mets,field.skill)
#' saveRDS(comb.skill,file=file.path(base.dir,PE.cfg$files$skill.metrics))


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
