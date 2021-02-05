#'========================================================================
# B2.Rolling_means.r
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Jan 22 23:50:07 2021
#
# <Description>
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
cat(sprintf("\n%s\n","B2.Rolling_means.r"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];


suppressPackageStartupMessages({
  library(PredEng)
  library(RcppRoll)
})

pcfg <- PE.load.config()

#'========================================================================
# Configure ####
#'========================================================================
rm.windows <- c(3,5,7,9)

#'========================================================================
# Setup ####
#'========================================================================

#Delete existing results 
log_msg("Getting list of previous ids to clear...")
existing.rm.SQL <- 
  sprintf("SELECT pKey FROM %s WHERE `resultName` LIKE '%%RollMean%%'",
          PE.cfg$db$stats)
this.query.time <- 
  system.time({
    del.these.pKeys <- 
      PE.db.getQuery(pcfg,PE.cfg$db$stats,existing.rm.SQL) %>%
      pull()
  })
log_msg("Complete in %0.3fs. \nDeleting...",this.query.time[3])
this.query.time <- 
  system.time({
    n <- PE.db.delete.by.pKey(object=pcfg,
                              table=PE.cfg$db$stats,
                              pKeys = del.these.pKeys)
  })
log_msg("Deleted %i rows in %0.3fs.\n\n",n,this.query.time[3])

#'========================================================================
# And Go ####
#'========================================================================
#'#Open database
stats.tbl <- PE.db.tbl(pcfg,PE.cfg$db$stats)

#'Load data
dat.in <- 
  stats.tbl  %>%
  filter(is.na(field)) %>%
  collect() %>%
  mutate(date=ymd(date)) %>%
  filter(month(date) %in% pcfg@MOI) %>%  #Drop unnecessary obs
  group_by(srcType,srcName,calibrationMethod,realization,startDate,
         spName,statName,resultName,.drop=TRUE) %>%
  arrange(date) %>%
  select(-pKey,-leadIdx,-field)
  
for(n in rm.windows) {
  log_msg("Rolling window %i...\n",n)
  #Calculate rolling means
  rm.stats <-
    dat.in  %>%
    mutate(date=as.character(date),
           value=roll_mean(value,n=n,fill=NA,align="center"),
           resultName=sprintf("%s/RollMean%i",resultName,n))
  
  #Write back to table
  PE.db.appendTable(pcfg,PE.cfg$db$stats,rm.stats)
}

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
dbDisconnect(stats.tbl)
if(length(warnings())!=0) print(warnings())
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
