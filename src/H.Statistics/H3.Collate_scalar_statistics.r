#'========================================================================
# H2. Collate statistics
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Jun  1 15:53:49 2018
#
# Collates statistics generated previously into a single file. Note 
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
cat(sprintf("\n%s\n","Collate salar statistics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
start.time <- proc.time()[3];

#Helper functions, externals and libraries
library(PredEng)
library(tidyverse)
#library(udunits2)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configure ####
#'========================================================================
stat.cfg.fname <- file.path(pcfg@scratch.dir,PE.cfg$files$stats.configuration)

#Directory setup
base.dir <- pcfg@scratch.dir
out.dir <- define_dir(base.dir,PE.cfg$dirs$collated.stats)

#'========================================================================
# Select input data ####
#'========================================================================
log_msg("Loading input data..\n")

#Retrieve configurations
stats.cfg <- 
  readRDS(stat.cfg.fname) %>%
  unnest(data) %>%
  #Retain results files that exist
  mutate(file.exists=file.exists(file.path(pcfg@scratch.dir,"Statistics",res.fname))) %>%
  filter(file.exists)

#Iterate over files
stats.l <- list()
pb <- progress_estimated(nrow(stats.cfg))
for(f in stats.cfg$res.fname){
  #Load the data, but drop the field statistics, 
  #to avoid things getting out of control size-wise
  stat.fname <- file.path(pcfg@scratch.dir,"Statistics",f)
  stats.l[[f]]   <- 
    readRDS(stat.fname) %>%
    select(-field)
  pb$tick()$print()
}
Sys.sleep(0.1)
pb$stop()

#Merge into one big object and add meta information
all.scalars <- 
  bind_rows(stats.l) %>%
  mutate(ym=sprintf("%i-%02i",year(date),month(date))) 

#'========================================================================
# Persistence forecasts ####
#'========================================================================
log_msg("\nSetting up persistence forecasts...\n")
#Extract persistence and observation data
obs.stats <- subset(all.scalars,src.type=="Observations")
persis.stats <- subset(all.scalars,src.type=="Persistence") %>%
              dplyr::select(-start.date)  %>%
              mutate(ym.date=sprintf("%i-%02i",year(date),month(date))) 

#Generate the forecast grid
forecast.dates <- filter(tibble(date=unique(obs.stats$date)),
                         year(date) %in% pcfg@comp.years)

persis.forecast.grid <- 
  expand.grid(date=forecast.dates$date,
              sp.subdomain=names(pcfg@spatial.domains),
              lead=pcfg@persistence.leads) %>%
  as_tibble() %>%
  mutate(sp.subdomain=as.character(sp.subdomain),
         start.date=date-months(lead),
         ym.start=sprintf("%i-%02i",year(start.date),month(start.date)))

persis.forecast.stats <- 
  left_join(persis.forecast.grid,
            persis.stats,
            by=c("ym.start"="ym.date","sp.subdomain") ) %>%
  mutate(date=date.x,
         date.x=NULL,date.y=NULL,ym.start=NULL,lead=NULL,
         ym=sprintf("%i-%02i",year(date),month(date)))

#Add it back to the stat list for output
out.stats <- 
  filter(all.scalars,src.type!="Persistence") %>%
  bind_rows(persis.forecast.stats)

#'========================================================================
# Complete ####
#'========================================================================
#Save results
saveRDS(out.stats, file=file.path(base.dir,PE.cfg$files$scalar.stats))

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
