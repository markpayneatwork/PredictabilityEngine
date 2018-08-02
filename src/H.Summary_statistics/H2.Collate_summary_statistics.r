#'========================================================================
# H2. Collate summary statistics
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Jun  1 15:53:49 2018
#
# Collates summary statistics generated previously into a single metric
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
cat(sprintf("\n%s\n","H2. Collate summary statistics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tibble)
library(dplyr)
library(reshape2)
library(lubridate)
library(udunits2)
load("objects/configuration.RData")
load("objects/PredEng_config.RData")
  
#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.debug.level(0)  #Non-zero lets us run with just a few points
} else {
  #Do everything
  set.debug.level(0)  #0 complete fresh run
}

#Directory setup
base.dir <- pcfg@scratch.dir

#'========================================================================
# Select input data ####
#'========================================================================
#Get list of sp subdomains
if(pcfg@use.global.ROI) {
  sp.subdomains <- ""
} else {
  sp.subdomains <- names(pcfg@spatial.subdomains)
}

#Loop over subdomains them individually and import
ss.l <- list()
for(sp in sp.subdomains){
  ss.fnames <- dir(file.path(base.dir,sp,"Summary.statistics"),full.names = TRUE)
  for(f in ss.fnames){
    var.names <- load(f)
    ss.l[[f]]   <- get(var.names)
  }
  
}

#Big improvements to the metadata handling have rendered previous hard-earned code 
#largely useless - we can now just take what we want
keep.cols <- c("sp.subdomain","name", "type",
               "date","start.date",
               "sumstat.data.type","sumstat.name","sumstat.type","value")
common.cols.l <- lapply(ss.l,"[",keep.cols)

#Merge into one big object and add meta information
all.ss.raw <- bind_rows(common.cols.l) %>%
               mutate(ym=sprintf("%i-%02i",year(date),month(date))) 

#'========================================================================
# Persistence forecasts ####
#'========================================================================
#Extract persistence and observation data
obs.ss <- subset(all.ss.raw,type=="Observations")
persis.ss <- subset(all.ss.raw,type=="Persistence") %>%
              select(-start.date)  %>%
              mutate(ym.date=sprintf("%i-%02i",year(date),month(date))) 

#Generate the forecast grid
lead.times <- c(1:11,seq(7,127,by=12))
#lead.times <- 1:120
persis.forecast.grid <- expand.grid(date=unique(obs.ss$date),
                                    lead=lead.times) %>%
                        as.tibble() %>%
                        mutate(start.date=date-months(lead),
                               ym.start=sprintf("%i-%02i",year(start.date),month(start.date)))
persis.forecast.ss <- left_join(persis.forecast.grid,
                                 persis.ss,
                                 by=c("ym.start"="ym.date") ) %>%
                        mutate(date=date.x,
                               date.x=NULL,date.y=NULL,ym.start=NULL,lead=NULL,
                               ym=sprintf("%i-%02i",year(date),month(date)))

#Add it back to the sumstat list
all.ss <- rbind(subset(all.ss.raw,type!="Persistence"),
                 persis.forecast.ss)

#Calculate lead time using udunits
ud.from <- "days since 1970-01-01"
ud.to <- "months since 1900-01-01"
all.ss$lead.raw <- as.numeric(ud.convert(all.ss$date,ud.from,ud.to))-
                    as.numeric(ud.convert(all.ss$start.date,ud.from,ud.to))
all.ss$lead <- round(all.ss$lead.raw/0.5)*0.5

#'========================================================================
# Split and Merge ####
#'========================================================================
#Drop years that are not to be included in the evaluation of skill metrics
#and drop CMIP5 as well (not interested in the skill)
sel.res <-  all.ss %>% 
            filter(year(date) %in% pcfg@comp.years,
                   !grepl("CMIP5",type)) 

#Extract out the observational data
obs.dat <- obs.ss %>% select(ym,sumstat.name,value)

#And merge it back into the comparison dataframe. This way we have both the
#modelled and the observed results together in the same dataframe. We note
#that we do the merging by year - this should generally be ok for most of the
#situations where we envisage using PredEnd i.e. one data point per year - but
#we need to be aware that this is not exactly the case 
comp.dat <- left_join(sel.res,obs.dat,
                      by=c("ym","sumstat.name"),
                      suffix=c(".mdl",".obs"))

#'========================================================================
# Calculate the metrics ####
#'========================================================================
#Now calculate the metrics
RMSE <- function(x,y) { sqrt(mean((x-y)^2))}
skill.m <- comp.dat %>%
           group_by(name,type,sumstat.name,lead) %>%
           summarize(cor=cor(value.mdl,value.obs,use="pairwise.complete"),
                     RMSE=RMSE(value.mdl,value.obs))


#'========================================================================
# Complete ####
#'========================================================================
#Save results
save(skill.m, file=file.path(base.dir,"Skill_metrics.RData"))
save(all.ss, file=file.path(base.dir,"All_sumstats.RData"))


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
