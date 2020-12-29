#'========================================================================
# B1. Mean adjustment
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Fri Aug 14 16:37:26 2020
#
# Performs a recalibration of all model data based on a simple mean adjustment
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
cat(sprintf("\n%s\n","B1. Mean adjustment"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressMessages({
  library(PredEng)
  library(lubridate)
  library(furrr)
  library(forcats)
})
pcfg <- readRDS(PE.cfg$path$config)

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

#Setup parallelism
if(Sys.info()["nodename"]=="aqua-cb-mpay18") {
  n.cores <- availableCores()
} else {
  n.cores <- as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))    
  assert_that(!is.na(n.cores),msg = "Cannot detect number of allocated cores")
}
plan(multisession,workers = n.cores)
options(future.globals.onReference = "error")

#'========================================================================
# Import data ####
#'========================================================================
log_msg("Import data..\n")
#Setup databases
this.db <- PE.db.connection(pcfg)
extr.tbl <- tbl(this.db,PE.cfg$db$extract)
clim.tbl <- tbl(this.db,PE.cfg$db$climatology)

#Clear all previous analyses that give these types of calibration methods
del.this <-
  tbl(this.db,PE.cfg$db$calibration) %>%
  filter(calibrationMethod %in% c("anomaly","MeanAdj","MeanVarAdj")) %>%
  select(pKey) %>%
  collect() %>%
  pull(pKey) 
PE.db.delete.by.pKey(pcfg,tbl.name=PE.cfg$db$calibration,del.this)

#Import observational climatology data for the month in question
#Note that the extraction process has already ensured for all of the models
#that the only thing in the extraction is the month of interest. However, for
#the observations, we have all months present. However, in this case we want to
#adjust the anomaly to the climatological observed value in the MOI. We therefore only
#need one value here.
#TODO: When implementing multiple MOIs, would extract relevant MOIs and average, I guess
obs.clim <-
  clim.tbl %>%
  filter(srcType=="Observations") %>%
  select(-pKey) %>%
  collect() %>% 
  PE.db.unserialize() %>%
  pivot_wider(names_from=statistic,values_from=field) %>%
  select(month,obsMean=mean,obsSd=sd)

#Import climatology data from both observations and model forecasts
clim.dat <-
  clim.tbl %>%
  select(-pKey) %>%
  collect() %>% 
  PE.db.unserialize() %>% 
  pivot_wider(names_from=statistic,values_from=field)%>%
  rename(mdlClim.mean=mean,mdlClim.sd=sd)

#Get list of extraction pKeys
extr.pKey <-
  extr.tbl %>%
  select(pKey) %>%
  collect() %>%
  pull()

#'========================================================================
# Calibration function####
#'========================================================================
calibration.fn <- function(this.dat,this.clim) {
  #Debugging
  # this.dat <- chunk.l[[1]]
  # this.clim <- obs.clim
  suppressMessages({
    library(raster)
  })
  
  #Apply recalibration
  rtn <-
    #Merge in observational climatology for the relevant month
    this.dat %>%
    left_join(y=this.clim,by="month") %>%
    #Calculate the anomaly and make the correction
    mutate(field.anom=map2(field.extr,mdlClim.mean,~ .x - .y),
           field.meanAdjust=map2(field.anom,obsMean,~ .x + .y),
           field.meanvarAdjust=pmap(list(anom=field.anom, 
                                         mdl.sd=mdlClim.sd, 
                                         obs.sd=obsSd, 
                                         obs.mean=obsMean),
                                    function(anom,mdl.sd,obs.sd,obs.mean) {
                                      ( anom / mdl.sd)*obs.sd+obs.mean}))
  return(rtn)
}

#'========================================================================
# Apply recalibration ####
#'========================================================================
#' To avoid conflicts with too many processes trying to write to the database
#' at the same time, we batch the process up into chunks and then use a parallelised apply process
#Loop over Source Files
log_msg("Applying recalibration...\n")

#Now loop over the chunks in a parallelised manner
chunk.size <- 100  #pKeys
basket.size <- chunk.size * n.cores
n.baskets <- ceiling(length(extr.pKey) / basket.size)
basket.l <- split(extr.pKey,rep(1:n.baskets,
                                each=basket.size,
                                length.out=length(extr.pKey)))
pb <- PE.progress(n.baskets)
dmp <- pb$tick(0)

for(this.basket in basket.l) {
  #Import data from extraction table
  extr.dat <-
    extr.tbl %>%
    filter(pKey %in% !!this.basket) %>%
    collect()    %>%
    PE.db.unserialize() %>%
    #Calculate date information
    mutate(date=ymd(date),
           month=month(date)) %>%
    rename(field.extr=field)
  
  #Merge in climatological data  
  merged.dat <- 
    extr.dat %>%
    #Join in model climatological data
    left_join(y=clim.dat,
              by=c("srcName","srcType","month","leadIdx")) 
  
  
  #Split basket into chunks
  chunk.l <- 
    merged.dat %>%
    mutate(batch.id=rep(seq(nrow(.)),
                        each=chunk.size,
                        length.out=nrow(.))) %>%
    group_by(batch.id) %>%
    group_split(.keep=FALSE)
  
  #Using Furrr and future to do the raster-based calculations
  calib.dat <-
    future_map_dfr(chunk.l,
                   calibration.fn,
                   this.clim=obs.clim,
                   .options = furrr_options(stdout=FALSE,
                                            seed=TRUE))
  
  #Write results
  out.dat <- 
    calib.dat %>%
    #Select columns and tidy
    select(srcName,srcType,realization,startDate,date,leadIdx,
           field.anom,field.meanAdjust,field.meanvarAdjust) %>% 
    mutate(date=as.character(date)) %>%
    rename("anomaly"=field.anom,
           "MeanAdj"=field.meanAdjust,
           "MeanVarAdj"=field.meanvarAdjust) %>%
    #Pivot
    pivot_longer(-c(srcName,srcType,realization,startDate,date,leadIdx),
                 names_to = "calibrationMethod",
                 values_to = "field") %>%
    #Drop unsupported calibrationMethods
    filter(calibrationMethod %in% pcfg@calibrationMethods)
  
  #Write results
  PE.db.appendTable(out.dat,pcfg,PE.cfg$db$calibration)
  
  #Loop
  pb$tick()
}


#'========================================================================
# Complete ####
#'========================================================================
#Finished 
dbDisconnect(this.db)

#Turn off the lights
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
