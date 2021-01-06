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
log_msg("Import data...\n")

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
         date=round_date(date,"year")) %>%
  group_by(srcType,srcName,realization,startDate,date) %>%
  summarise(NAO=mean(NAO),
            n=n(),
            .groups="drop") %>%
  #Drop partials
  filter(n==length(pcfg@MOI)) %>%
  #Calculate lead times
  mutate(startDate=as.Date(startDate),
         lead.yrs=round(as.numeric(difftime(date,startDate,units="days"))/365))

#'========================================================================
# Evaluate signal ratios ####
#'========================================================================
log_msg("Evaluate signal ratios...\n")

#Calculate signal ratios
NAO.stats <- 
  NAO.dat %>%
  #Focus on realmeans and observations in the comparison years
  filter(realization=="realmean" | srcType=="Observations") %>%
  filter(year(date) %in% pcfg@comp.years) %>%
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
#There are multiple ways that we could associate the NAO index with the particular forecast
#variable in question. These include
#  * round to previous winter
#  * round to subsequent winter
#  * taking bracketing winters
#  * winters to date
# As it's not immediately clear which of these gives the best performance, we simply
# try them all, and then do the NAO matching accordingly
log_msg("Calculating rankings...\n")

#Adjust realmeans to have correct magnitude
NAO.realmean <- 
  #Add in rps
  NAO.dat %>%
  filter(realization=="realmean") %>%
  left_join(y=rps,by=c("srcType","srcName","lead.yrs")) %>%
  #Adjust magnitude
  mutate(adj.realmean=NAO*rps) %>%
  select(srcType,srcName,startDate,date,adj.realmean) 
  
#Calculate errors
real.err <- 
  #Merge into realisation members
  NAO.dat %>%
  filter(srcType!="Observations",
         !realization %in% c("realmean","ensmean")) %>%
  left_join(y=NAO.realmean,by=c("srcType","srcName","startDate","date")) %>%
  mutate(abserr=abs(NAO-adj.realmean)) %>% 
  select(-n,-NAO,-adj.realmean)

#Now apply the ranking algorithms
rank.l <- list()
rank.l$previous <- 
  real.err %>%
  group_by(srcType,srcName,startDate,date) %>%
  mutate(rank=rank(abserr)) %>%
  mutate(match.method="PreviousWinter") %>%
  ungroup() 
  
rank.l$nextWinter <-
  #Requires that we remerge the realmeans in again, but this time matching with the following winter
  NAO.dat %>%
  filter(srcType!="Observations",
         !realization %in% c("realmean","ensmean")) %>%
  mutate(nextWinter=date+years(1)) %>%
  left_join(y=NAO.realmean,by=c("srcType","srcName","startDate",nextWinter="date")) %>%
  mutate(abserr=abs(NAO-adj.realmean)) %>% 
  select(-n) %>%
  #Now generate rankings
  group_by(srcType,srcName,startDate,date) %>%
  mutate(rank=rank(abserr)) %>%
  mutate(match.method="NextWinter") %>%
  ungroup() %>%
  #Tidy
  select(-NAO,-adj.realmean,-nextWinter)

#Generic function to filter and rank rmse by specified leads
lead.rank <- 
  function(leads,dat) {
    #Filter by leads first
    rank.this <-
      dat %>%
      filter(lead.yrs %in% leads) 
    
    #Then calculate the rmse errors and ranks
    these.ranks <-
      rank.this %>%
      group_by(srcType,srcName,startDate,realization) %>%
      summarise(dates=list(date),
                rmse=sqrt(mean(abserr^2)),
                .groups="drop") %>%
      group_by(srcType,srcName,startDate)%>%
      mutate(rank=rank(rmse)) %>%
      ungroup()
  }

#Ranking based on brackets ie rmse on either side of the desired date
rank.l$bracket <- 
  #Do bracketing by lead times
  lapply(0:9,function(i) c(i,i+1)) %>%
  lapply(lead.rank,dat=real.err) %>%
  bind_rows() %>%
  #Tidy
  mutate(date=map(dates,~.x[[1]]),
         match.method="bracketing") %>%  #Use first date as being representative
  unnest(c(date=date)) %>%
  select(-dates,-rmse)

#Ranking based on NAO rmse to next date
#Note that we require bracketing of the time interval of interest, so the last forecast
#gets shaved off. This could be fixed if necessary.
rank.l$todate <- 
  lapply(0:8,function(i) seq(from=0,to=i+1)) %>%
  lapply(lead.rank,dat=real.err) %>%
  bind_rows() %>%
  #Tiday
  mutate(date=map(dates,~tail(.x,2)[[1]]), #Take second last date as representative
         match.method="todate") %>%
  unnest(c(date=date)) %>%
  select(-dates,-rmse)

#And combine
rank.all <-
  rank.l %>%
  bind_rows()

#'========================================================================
# Complete ####
#'========================================================================
saveRDS(rank.all,file=PE.cfg$path$NAOmatching)
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
