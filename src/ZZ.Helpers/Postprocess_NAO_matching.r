#'========================================================================
# Process_NAO_matching
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed Dec 30 14:53:59 2020
#
# Post processes data from NAO predictions to generate a ranking of
# realisations that can be subsequented used to do ensemble member
# selection. Impementation of the Smith et al 2020 Nature algorithm
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
cat(sprintf("\n%s\n","Process_NAO_matching"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressMessages({
  library(PredEng)
})

#'========================================================================
# Configure ####
#'========================================================================
#Specific configs - hardwired
pcfg <- readRDS(here("scratch/NAO_matching/configuration.rds"))
db.path <- here(pcfg@scratch.dir,"NAO_matching.sqlite")

#'========================================================================
# Import data ####
#'========================================================================
log_msg("Import data..\n")

#Setup databases
src.db <- dbConnect(RSQLite::SQLite(), db.path)
stats.tbl <- tbl(src.db,PE.cfg$db$stats)
extr.tbl <- tbl(src.db,PE.cfg$db$extract)
calib.tbl <- tbl(src.db,PE.cfg$db$calib)
#Import stats
stat.dat <-
  stats.tbl %>%
  filter(statName=="MeanSLP",
         resultName=="average",
         calibrationMethod=="anomaly") %>%
  select(-pKey,-field,-leadIdx) %>%
  collect() 
dbDisconnect(src.db)

#Calculate NAO indices
NAO.dat <-
  stat.dat %>%
  #Restrict to Boreal winter
  mutate(date=as.Date(date),
         month=month(date)) %>%
  filter(month %in% pcfg@MOI) %>%
  #Calculate index
  pivot_wider(names_from="spName",
              values_from = "value") %>%
  mutate(NAO=Azores-Iceland) %>%
  #Average over the winter period, defined here as Dec, Jan, Feb, Mar.
  #We do this by rounding the date to the nearest 1 Jan and then averaging
  mutate(date=as.Date(date),
         NAOyear=round_date(date,"year")) %>%
  group_by(srcType,srcName,realization,startDate,NAOyear) %>%
  summarise(NAO=mean(NAO),
            n=n(),
            .groups="drop") %>%
  #Drop partials
  filter(n==length(pcfg@MOI)) %>%
  #Calculate lead times
  mutate(startDate=as.Date(startDate),
         lead.yrs=round(as.numeric(difftime(NAOyear,startDate,units="days"))/365))

#'========================================================================
# Evaluate signal ratios ####
#'========================================================================
#Calculate signal ratios
NAO.stats <- 
  NAO.dat %>%
  #Focus on realmeans and observations in the comparison years
  filter(realization=="realmean" | srcType=="Observations") %>%
  filter(year(NAOyear) %in% pcfg@comp.years) %>%
  #Calculate standard deviations
  group_by(srcType,srcName,lead.yrs) %>%
  summarise(mean=mean(NAO),
            sd=sd(NAO),
            n=n(),
            .groups="drop")

NAO.obs.sd <- 
  NAO.stats %>%
  filter(srcType=="Observations") %>%
  pull(sd)

rps <- 
  NAO.stats %>%
  mutate(rps=NAO.obs.sd/sd)

#'========================================================================
# Rank realisations ####
#'========================================================================
#Adjust realmeans to have correct magnitude
NAO.realmean <- 
  #Add in rps
  NAO.dat %>%
  filter(realization=="realmean") %>%
  left_join(y=rps,by=c("srcType","srcName","lead.yrs")) %>%
  #Adjust magnitude
  mutate(NAO.adj=NAO*rps) %>%
  select(srcType,srcName,startDate,NAOyear,NAO.adj) 
  
#Calculate rankings
real.rank <- 
  #Merge into realisation members
  NAO.dat %>%
  filter(srcType!="Observations",
         !realization %in% c("realmean","ensmean")) %>%
  left_join(y=NAO.realmean,by=c("srcType","srcName","startDate","NAOyear")) %>%
  #Calculate errors
  mutate(err=NAO-NAO.adj) %>%
  group_by(srcType,srcName,realization,startDate) %>%
  summarise(rmse=sqrt(mean(err^2)),
            .groups="keep") %>%
  #Generate rankings
  group_by(srcType,srcName,startDate) %>%
  mutate(rank=rank(rmse)) %>%
  ungroup()

#'========================================================================
# Complete ####
#'========================================================================
saveRDS(real.rank,file="objects/NAO_matching_ranking.rds")
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
