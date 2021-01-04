#'========================================================================
# B2.NAO_matching
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Mon Jan  4 15:04:02 2021
#
# Implements the NAO matching algorithm of Smith et al 2020 Nature. 
# Note that for this script to be applied, it first needs B7. NAO matching
# to be run and the outputs to be postprocessed using 
#   * src/ZZ.Helpers/Postprocess_NAO_matching.r
#
# NAO matching could be implemented at several stages of the workflow - it
# could in principle even be used to reduce the amount of data that is extracted
# from e.g. CESM.DPLE. However, it is also quite useful to be able to compare the
# effect of NAO matching to model outputs without it. We therefore start by including
# it as a calibrationMethod in the first instance, meaning that we also need to
# recalculate the appropriate realisation means as well.
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
cat(sprintf("\n%s\n","B2.NAO_matching"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressMessages({
  library(PredEng)
  library(lubridate)
})
pcfg <- PE.load.config()

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.log_msg.silent()
} else {
  #Do everything and tell us all about it
  set.log_msg.silent(FALSE)
}

#Number of realisations to consider
n.reals <- c(5)

#'========================================================================
# Prepare data ####
#'========================================================================
log_msg("Import data..\n")

#NAO matching database
NAO.ranking <- 
  readRDS(PE.cfg$path$NAOmatching) %>%
  select(-abserr,-lead.yrs) %>%
  mutate(date=year(date), #Only interested in the year part of the date
         startDate=as.character(startDate)) 

#Setup databases
this.db <- PE.db.connection(pcfg)
calib.tbl <- tbl(this.db,PE.cfg$db$calibration)

#Clear all previous analyses that give these types of calibration methods
del.this <-
  calib.tbl %>%
  filter(substr(calibrationMethod,1,11) =="NAOmatching") %>%
  select(pKey) %>%
  collect() %>%
  pull(pKey) 
PE.db.delete.by.pKey(pcfg,tbl.name=PE.cfg$db$calibration,del.this)

#Which data sets and start dates can we apply NAO matching to?
extract.cols <- c("srcType","srcName","startDate")
can.NAOmatch <- 
  NAO.ranking %>%
  unite("extract.key",all_of(extract.cols))%>%
  distinct(extract.key) %>%
  pull()

#Select calibrations where we can potentially do this
NAOmatch.candidates <- 
  calib.tbl %>%
  mutate(extract.key=paste(srcType ,srcName,startDate,sep="_")) %>%
  filter(extract.key %in% can.NAOmatch,
         realization != "realmean",
         calibrationMethod=="MeanAdj") %>%   #Only take meanAdjusted
  select(-field,-extract.key) %>%
  collect() %>%
  mutate(date=year(date)) 

#'========================================================================
# Apply matching algorithm ####
#'========================================================================
#Merge in rankings
#Note that we do the matching by year only, dropping the moth and day terms
ranked.candidates <- 
  NAOmatch.candidates %>%
  left_join(y=NAO.ranking,by=c(extract.cols,"realization","date")) %>%
  #Rerank, just to make sure
  group_by(srcType,srcName,startDate,date,match.method) %>%
  mutate(rank.old=rank,
         rank=rank(rank.old)) %>%
  ungroup()

#Now it's time to do it
for(n in n.reals) {
  #Select candidates
  these.sel <- 
    ranked.candidates %>%
    filter(rank<=n)
  
  #Compress into list of pKeys
  pKey.list <-
    these.sel %>%
    group_by(match.method) %>%
    summarise(pKey=list(pKey),
              .groups="drop") %>%
    mutate(calibrationMethod=paste("NAOmatching",match.method,n,sep="."))
  
  #Copy pKeys
  pKey.list %>%
    rowwise() %>%
    group_walk(function(this,...) {
      this.sel<-
        calib.tbl %>%
        filter(pKey %in% !!this$pKey[[1]]) %>%
        collect() %>%
        mutate(calibrationMethod=!!this$calibrationMethod)  %>%
        select(-pKey)
      PE.db.appendTable(this.sel,pcfg,PE.cfg$db$calibration)
    })
}

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
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
