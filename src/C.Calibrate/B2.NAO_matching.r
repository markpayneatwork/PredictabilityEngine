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
  library(furrr)
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

#Configuration
n.members <- c(5,10,15,20,40)
match.methods <- c("NextWinter","PreviousWinter","bracketing","todate")
# n.members <- c(10)
# match.methods <- c("bracketing")

#Setup parallelisation
if(Sys.info()["nodename"]=="aqua-cb-mpay18" | interactive()) {
  n.cores <- availableCores()
} else {
  n.cores <- as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))    
  assert_that(!is.na(n.cores),msg = "Cannot detect number of allocated cores")
}
plan(multisession,workers = n.cores)

stop("Needs to be updated to parallel tracks")
#'========================================================================
# Prepare data ####
#'========================================================================
log_msg("Import data..\n")

#NAO matching database
NAO.ranking <- 
  readRDS(PE.cfg$path$NAOmatching) %>%
  select(-abserr,-lead.yrs) %>%
  mutate(date=year(date), #Only interested in the year part of the date
         startDate=as.character(startDate)) %>%
  filter(match.method %in% match.methods)

#Setup databases
this.db <- PE.db.connection(pcfg,PE.cfg$db$calibration)
calib.tbl <- tbl(this.db,PE.cfg$db$calibration)

#Clear all previous analyses that give these types of calibration methods
del.this <-
  calib.tbl %>%
  filter(substr(calibrationMethod,1,11) =="NAOmatching") %>%
  select(pKey) %>%
  collect() %>%
  pull(pKey) 
PE.db.delete.by.pKey(pcfg,PE.cfg$db$calibration,del.this)

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
log_msg("Select members...\n")
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
for(n in n.members) {
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
    mutate(calibrationMethod=sprintf("NAOmatching.%s.%02i",match.method,n))
  
  #Copy pKeys
  pKey.list %>%
    rowwise() %>%
    group_walk(function(this,...) {
      #Extract
      this.sel<-
        calib.tbl %>%
        filter(pKey %in% !!this$pKey[[1]]) %>%
        collect() %>%
        mutate(calibrationMethod=!!this$calibrationMethod)  %>%
        select(-pKey)  
      #Then rewrite
      #Note that we save a bit of time by skipping the unserialization step.
      PE.db.appendTable(pcfg,PE.cfg$db$calibration,this.sel,serialize.first=FALSE)
    })
}

#'========================================================================
# Calculate realmeans ####
#'========================================================================
log_msg("Calculate realisation means using %i cores...\n",n.cores)
#Realisation means are normally calculated after extraction, and then 
#calibrated from there. This is ok, as calibration is typically a linear
#transformation. However, that wouldn't work for this approach, and so we
#recalculate the realisation means as well
#Extract data and perform averaging
chunk.meta <-
  calib.tbl %>%
  filter(substr(calibrationMethod,1,11) =="NAOmatching") %>%
  select(pKey,srcType,srcName,realization,calibrationMethod,startDate,date,leadIdx) %>%
  collect() %>%
  group_by(across(-c(pKey,realization)),.drop=TRUE) %>%
  mutate(grp.idx=cur_group_id())

#Divide into baskets
n.chunks <- n_groups(chunk.meta)
basket.size <- 10*n.cores
n.baskets <- ceiling(n.chunks / basket.size)
basket.l <- split(1:n.chunks,rep(1:n.baskets,
                                each=basket.size,
                                length.out=n.chunks))
pb <- PE.progress(n.baskets)
dmp <- pb$tick(0)

for(this.basket in basket.l) {
  #Resolve chunks
  these.pKeys <-
    chunk.meta %>%
    filter(grp.idx %in% this.basket) %>%
    pull(pKey)
  
  #Setup data
  these.chunks <-
    #Import fields
    calib.tbl %>%
    filter(pKey %in% these.pKeys) %>%
    collect() %>%
    PE.db.unserialize() %>%
    #Nest into chunks
    select(-pKey,-realization) %>%
    nest(field.tbl=c(field)) %>%
    mutate(field.l=map(field.tbl,pull,"field")) 
    
  #Process in parallel
  realMeans <-
    these.chunks %>%
    mutate(field=future_map(field.l,
                            ~ mean(brick(.x)),
                            .options = furrr_options(stdout=FALSE,
                                                     seed=TRUE)),
           
           realization="realmean") %>%
    select(-field.l,-field.tbl)
  
  #Write to database 
  realMeans %>%
    PE.db.appendTable(pcfg, PE.cfg$db$calibration,dat=.)
  
  #Loop
  pb$tick()
}



#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
plan(sequential)
dbDisconnect(this.db)
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
