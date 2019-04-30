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
# Collates summary statistics generated previously into a single metric. Note 
# that the collation takes place over two dimensions - firstly over the data
# sources (e.g. NMME, Decadal, Observations) and then this needs to be 
# repeated for for each spatial area.
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
#library(udunits2)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
   #partition.collation <- TRUE
  # cfg.no <- 2
} else {
  # #Taking inputs from the system environment
  # partition.collation <- TRUE
  # cfg.no <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  # if(cfg.no=="") stop("Cannot find LSB_JOBINDEX")
}

#Retrieve configurations
#However, in some cases we will want to do the collation all in one step.
#In other cases, we will want to partition it up into smaller bits
#We make the distinction based on whether the script is being run
#interactively or from a script
# if(partition.collation){
#   cfg.fname <- file.path(PE.cfg$dirs$cfg,"SumStat_Collate.cfg")
#   this.cfgs <- get.this.cfgs(cfg.fname)
#   this.sp <- get.this.sp(cfg.fname,cfg.no,pcfg)
#   sp.dirs <- this.sp@name
#   config.summary(this.sp)
# } else 
  
if(pcfg@use.global.ROI) {
  sp.dirs <- ""
} else {
  sp.dirs <- names(pcfg@spatial.subdomains)
}

#Directory setup
base.dir <- pcfg@scratch.dir
out.dir <- define_dir(base.dir,"Collated.SumStats")

#'========================================================================
# Select input data ####
#'========================================================================
log_msg("Loading input data..\n")
#Loop over subdomains them individually and import
ss.l <- list()
for(sp.d in sp.dirs){
  ss.fnames <- dir(file.path(base.dir,sp.d,"Summary.statistics"),full.names = TRUE)
  for(f in ss.fnames){
    ss.l[[f]]   <- readRDS(f)
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
log_msg("Setting up persistence forecasts...\n")
#Extract persistence and observation data
obs.ss <- subset(all.ss.raw,type=="Observations")
persis.ss <- subset(all.ss.raw,type=="Persistence") %>%
              select(-start.date)  %>%
              mutate(ym.date=sprintf("%i-%02i",year(date),month(date))) 

#Generate the forecast grid
lead.times <- 1:120
forecast.dates <- filter(tibble(date=unique(obs.ss$date)),year(date) %in% pcfg@comp.years)
persis.forecast.grid <- expand.grid(date=forecast.dates$date,
                                    sp.subdomain=names(pcfg@spatial.subdomains),
                                    lead=lead.times) %>%
                        as.tibble() %>%
                        mutate(sp.subdomain=as.character(sp.subdomain),
                               start.date=date-months(lead),
                               ym.start=sprintf("%i-%02i",year(start.date),month(start.date)))
persis.forecast.ss <- left_join(persis.forecast.grid,
                                 persis.ss,
                                 by=c("ym.start"="ym.date","sp.subdomain") ) %>%
                        mutate(date=date.x,
                               date.x=NULL,date.y=NULL,ym.start=NULL,lead=NULL,
                               ym=sprintf("%i-%02i",year(date),month(date)))

#Add it back to the sumstat list
all.ss <- rbind(subset(all.ss.raw,type!="Persistence"),
                 persis.forecast.ss)

#Calculate lead time using udunits
# ud.from <- "days since 1970-01-01"
# ud.to <- "months since 1900-01-01"
# all.ss$lead.raw <- as.numeric(ud.convert(all.ss$date,ud.from,ud.to))-
#                     as.numeric(ud.convert(all.ss$start.date,ud.from,ud.to))

#Calculate lead time manually
date.to.months <- function(x) {
  #Months since 1 Jan 1900
  (year(x)-1900)*12 + (month(x)-1) + (day(x)-1)/days_in_month(x)
}
all.ss$lead.raw <- date.to.months(all.ss$date)- date.to.months(all.ss$start.date)
all.ss$lead <- round(all.ss$lead.raw/0.5)*0.5  #Half month accuracy

#' #'========================================================================
#' # Split and Merge ####
#' #'========================================================================
#' log_msg("Split and merge...\n")
#' 
#' #Drop years that are not to be included in the evaluation of skill metrics
#' #and drop CMIP5 as well (not interested in the skill)
#' sel.res <-  all.ss %>% 
#'             filter(year(date) %in% pcfg@comp.years,
#'                    !grepl("CMIP5",type)) 
#' 
#' #Extract out the observational data
#' obs.dat <- obs.ss %>% 
#'   filter(year(date) %in% pcfg@comp.years) %>%
#'   select(sp.subdomain,ym,sumstat.name,value) 
#' 
#' 
#' #And merge it back into the comparison dataframe. This way we have both the
#' #modelled and the observed results together in the same dataframe. We note
#' #that we do the merging by year - this should generally be ok for most of the
#' #situations where we envisage using PredEnd i.e. one data point per year - but
#' #we need to be aware that this is not exactly the case 
#' comp.dat <- left_join(sel.res,obs.dat,
#'                       by=c("ym","sumstat.name","sp.subdomain"),
#'                       suffix=c(".mdl",".obs"))

#'========================================================================
# Calculate the metrics ####
#'========================================================================
# log_msg("Metric calculation...\n")
# #Now calculate the metrics
# RMSE <- function(x,y) { sqrt(mean((x-y)^2,na.rm=TRUE))}
# skill.m <- comp.dat %>%
#            group_by(name,type,sumstat.name,lead,sp.subdomain) %>%
#            summarize(cor=cor(value.mdl,value.obs,use="pairwise.complete"),
#                      RMSE=RMSE(value.mdl,value.obs))


#'========================================================================
# Complete ####
#'========================================================================
#Save results
#save(skill.m, file=file.path(base.dir,"Skill_metrics.RData"))
saveRDS(all.ss, file=file.path(base.dir,"All_sumstats.rds"))


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
