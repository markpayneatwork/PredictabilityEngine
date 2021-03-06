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
pcfg <- PE.load.config()

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  this.srcType <- "Observations"
  this.srcName <- "HadISST"
} else {  #Running as a "function"
  cmd.args <- commandArgs(TRUE)
  assert_that(length(cmd.args)==2,msg="Cannot get command args")
  this.srcType <- cmd.args[1]
  this.srcName <- cmd.args[2]
}
this.datasrc <- data.source(type=this.srcType,name=this.srcName)
ref.datasrc <-  data.source(type="Observations",name=pcfg@reference)   #baseline reference to adjust to
this.datasrc.is.ref <- identical(this.datasrc,ref.datasrc)
PE.config.summary(pcfg,this.datasrc)

#Setup parallelism
if(Sys.info()["nodename"]%in% c("aqua-cb-mpay18","volta.dmi.dk") | interactive()) {
  n.cores <- 8
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
extr.tbl <- 
  PE.db.tbl(pcfg,PE.cfg$db$extract,this.datasrc)  %>%
  select(-srcFname)   #srcFname is only in extraction tables
clim.tbl <- PE.db.tbl(pcfg,PE.cfg$db$climatology,this.datasrc)
PE.db.setup.calibration(pcfg,this.datasrc)
calib.tbl <- PE.db.tbl(pcfg,PE.cfg$db$calibration,this.datasrc)

#Clear all previous analyses that give the types of calibration methods done here
del.this <-
  calib.tbl %>%
  filter(calibrationMethod %in% c("None","anomaly","MeanAdj","MeanVarAdj"),
         srcType==!!this.datasrc@type,
         srcName==!!this.datasrc@name) %>%
  select(pKey) %>%
  collect() %>%
  pull(pKey) 
PE.db.delete.by.pKey(pcfg,PE.cfg$db$calibration,this.datasrc,del.this)

#Import observational climatology data for the month in question
#Note that the extraction process has already ensured for all of the models
#that the only thing in the extraction is the month of interest. However, for
#the observations, we have all months present. In the case where we have only one MOI,
#we can adjust the anomaly to the climatological observed value in the MOI to produce an
#"anomaly persistence" type of forecast. However, this is very hard to interpret when we
#are starting to think about multiple MOIs - it might work ok though if thinking about
#averaged MOIs on the other hand. Anyway, it's a bit hard to handle at the moment, so we
#do not allow meanAdjustment when there are multiple MOIs at the moment.
ref.clim <-
  clim.tbl %>%
  filter(srcType==!!ref.datasrc@type,
         srcName==!!ref.datasrc@name) %>%
  select(-pKey) %>%
  collect() %>% 
  PE.db.unserialize() %>%
  pivot_wider(names_from=statistic,values_from=field) %>%
  select(month,obsMean=mean,obsSd=sd)

target.clim <-   #Climatological values which to do mean adjustment
  ref.clim %>%
  filter(month %in% pcfg@MOI) %>%
  #Although we don't currently use it, we can nevertheless average over the months here
  summarise(targetMean=list(mean(brick(obsMean))),
            n=n(),
            targetSd=list(1/n*sqrt(sum(brick(obsSd)^2)))) %>%
  unlist()  #Put it into a list format

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
calibration.fn <- function(this.dat,this.target,this.calib) {
  #Debugging
  # this.dat <- chunk.l[[1]]
  # this.target <- target.clim
  # this.calib <- pcfg@calibrationMethods
 
  rtn <- this.dat
  #We store the raw values if requested explicitly, or if we are working with the
  #reference datasource, do this
  if(any(this.calib == "None") | this.datasrc.is.ref) {
    rtn <- 
      rtn %>%
      mutate(calib.None=field.extr)
  }
  
  #Next calculate the anomaly - we always need this
  rtn <-
    rtn %>%
    mutate(calib.anomaly=map2(field.extr,mdlClim.mean,
                           ~ .x - .y))
  
  #Do the mean adjustment if required
  if(any(this.calib %in% c("MeanAdj","MeanVarAdj","NAOmatching"))) {
    rtn <-
      rtn %>%
      mutate(calib.MeanAdj=map(calib.anomaly, 
                                   ~ .x + this.target$targetMean))
  }
  
  #And the variance adjustment
  if(any(this.calib == "MeanVarAdj")) {
    rtn <-
      rtn %>%
      mutate(calib.MeanVarAdj=map2(calib.anomaly, mdlClim.sd,
                                      ~(.x/.y)*this.target$targetSd + this.target$targetMean))
  }

  #But drop the anomaly if not requested
  if(!any(this.calib=="anomaly")) {
    rtn <-
      rtn %>%
      select(-calib.anomaly)
  }

  return(rtn)
}

#'========================================================================
# Apply recalibration ####
#'========================================================================
#' To avoid conflicts with too many processes trying to write to the database
#' at the same time, we batch the process up into chunks and then use a parallelised apply process
#Loop over Source Files
log_msg("Applying recalibration using %i cores...\n",n.cores)

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
  
  #Merge in additional data  
  merged.dat <- 
    extr.dat %>%
    #Join in model climatological data
    left_join(y=clim.dat,
              by=c("srcName","srcType","month","lead")) %>%
    #Join in observations
    left_join(y=ref.clim,by="month")

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
                   this.target=target.clim,
                   this.calib=pcfg@calibrationMethods,
                   .options = furrr_options(stdout=FALSE,
                                            seed=TRUE))
  
  #Write results
  out.dat <- 
    calib.dat %>%
    #Select columns 
    select(srcName,srcType,realization,startDate,date,lead,
           starts_with("calib.")) %>% 
    #Pivot
    pivot_longer(starts_with("calib."),
                 names_to = "calibrationMethod",
                 values_to = "field") %>%
    #Tidy
    mutate(calibrationMethod=gsub("calib\\.","",calibrationMethod),
           date=as.character(date)) 
    
  #Write results
  PE.db.appendTable(pcfg,PE.cfg$db$calibration,this.datasrc,out.dat)
  
  #Loop
  pb$tick()
}


#'========================================================================
# Complete ####
#'========================================================================
#Finished 
dbDisconnect(calib.tbl)
dbDisconnect(clim.tbl)
dbDisconnect(extr.tbl)

#Turn off the lights
plan(sequential)
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
