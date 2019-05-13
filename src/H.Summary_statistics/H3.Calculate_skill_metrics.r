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
# Calculates skill metrics based on a supplied database of summary statistics
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
cat(sprintf("\n%s\n","Calculate_skill_metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

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

#Import Sumstat results
all.ss <- readRDS(file.path(base.dir,PE.cfg$files$sumstats))

#'========================================================================
# Split and Merge Observations ####
#'========================================================================
log.msg("Split and merge...\n")

#Extract out the observational data
obs.dat <- filter(all.ss,type=="Observations") %>%
  dplyr::select(sp.subdomain,ym,sumstat.name,value) 


#And merge it back into the comparison dataframe. This way we have both the
#modelled and the observed results together in the same dataframe. We note
#that we do the merging by year - this should generally be ok for most of the
#situations where we envisage using PredEnd i.e. one data point per year - but
#we need to be aware that this is not exactly the case 
comp.dat.all <- left_join(all.ss,obs.dat,
                          by=c("ym","sumstat.name","sp.subdomain"),
                          suffix=c(".mdl",".obs"))

#Subset further and add start.month
comp.dat <- filter(comp.dat.all,
                   year(date) %in% pcfg@comp.years) %>%
  mutate(start.month=month(start.date))

#Restrict the comparisons to the realmean-based metrics - for the moment
comp.dat <- filter(comp.dat,sumstat.use.realmeans)

#'========================================================================
# Calculate the metrics ####
#'========================================================================
#Looking to calculate a set of metrics here. In particular, we want to have
#the mean skill, but also the range across initialisation dates as well

log.msg("Metric calculation...\n")

#Skill functions
RMSE <- function(x,y) { sqrt(mean((x-y)^2,na.rm=TRUE))}
skill.sum <- function(d) {
  d %>%   summarize(cor=cor(value.mdl,value.obs,use="pairwise.complete"),
                    RMSE=RMSE(value.mdl,value.obs))
}

#Now calculate the mean skill over all start dates
g.vars <- c("name","type","sp.subdomain","sumstat.name","lead")
skill.mean <- comp.dat %>%
  group_by_at(vars(one_of(g.vars))) %>%
  skill.sum() %>%
  gather("skill.metric","value",-one_of(g.vars)) %>%
  mutate(skill.type="mean.skill")


#Now calculate the range of skill over start dates
skill.range <- comp.dat %>%
  group_by_at(vars(one_of(c(g.vars,"start.month")))) %>%
  skill.sum()%>%  
  gather("skill.metric","value",-one_of(c(g.vars,"start.month"))) %>%
  ungroup() %>%
  filter(!is.infinite(value)) %>%
  group_by_at(vars(one_of(c(g.vars,"skill.metric")))) %>%
  summarize(max.skill=max(value,na.rm=TRUE),
            min.skill=min(value,na.rm=TRUE)) %>%
  gather("skill.type","value",ends_with("skill"))

#Merge into one tibble and save results
skill.mets <- rbind(skill.range,skill.mean)
saveRDS(skill.mets,file=file.path(base.dir,PE.cfg$files$skill.metrics))


#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
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
