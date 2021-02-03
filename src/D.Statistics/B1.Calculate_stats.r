#'========================================================================
# B1.Calculate_stats
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed Aug 19 08:31:33 2020
#
# Calculates the individual statistics. This version is discretised into 
# handling a single stat and a single spatial polygon domain at the same
# time, and then iterating over all of the calibration fragments.
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
cat(sprintf("\n%s\n","B1.Calculate_stats"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressPackageStartupMessages({
  library(PredEng)
  library(furrr)
})
pcfg <- PE.load.config()

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.log_msg.silent()
  stat.id <- names(pcfg@statistics)[1]
  sp.id <- c(pcfg@spatial.polygons$name,PE.cfg$misc$globalROI)[1]
} else {
  set.log_msg.silent()
  cmd.args <- commandArgs(TRUE)
  assert_that(length(cmd.args)==2,msg="Cannot get command args")
  sp.id <- cmd.args[1]
  stat.id <- cmd.args[2]
}

#Setup parallelisation
if(Sys.info()["nodename"]=="aqua-cb-mpay18" | interactive()) {
  n.cores <- availableCores()
} else {
  n.cores <- as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))    
  assert_that(!is.na(n.cores),msg = "Cannot detect number of allocated cores")
}
plan(multisession,workers = n.cores)


#'========================================================================
# Setup  ####
#'========================================================================
#Fail gracefully
dmp <- assert_that(all(stat.id %in% names(pcfg@statistics)) & length(stat.id)==1,
            msg="Unknown stat(s) requested")
dmp <- assert_that(all(sp.id %in% c(PE.cfg$misc$globalROI,pcfg@spatial.polygons$name)) & 
                     length(sp.id)==1,
                   msg="Unknown spatial polygon(s) requested")

#Extract elements to process
this.stat <- pcfg@statistics[[stat.id]]
this.sp <- 
  if(sp.id == PE.cfg$misc$globalROI) {
    assert_that(this.stat@use.globalROI,msg="Spatial polygon is globalROI, but stat doesn't use one.")
    PE.global.sf(pcfg) 
  } else {
    pcfg@spatial.polygons %>% 
      filter(name==sp.id)
  }
dmp <- assert_that(nrow(this.sp)==1,msg="Failed to select only one spatial polygon")

#Display Configuration
PE.config.summary(pcfg,
                  "spName"=sp.id,
                  "statName"=stat.id,
                  "calibrationMethod(s)"=paste0(this.stat@calibration,collapse=", "),
                  "sources"=paste0(this.stat@sources,collapse=", "))
log_msg("\n\nSpatial domain--------------------------------------------\n")
print(this.sp)
log_msg("\n\nStatistic-------------------------------------------------\n")
print(this.stat)

#Delete existing results 
log_msg("Getting list of previous ids to clear...")
existing.stats.sel <- 
  sprintf("SELECT pKey FROM %s WHERE `statName` = '%s' AND `spName` = '%s'",
          PE.cfg$db$stats,
          stat.id,
          sp.id)
this.query.time <- 
  system.time({
    del.these.pKeys <- 
      PE.db.getQuery(pcfg,PE.cfg$db$stats,existing.stats.sel) %>%
      pull()
  })
log_msg("Complete in %0.3fs. \nDeleting...",this.query.time[3])
this.query.time <- 
  system.time({
    n <- PE.db.delete.by.pKey(pcfg,PE.cfg$db$stats,pKeys = del.these.pKeys)
  })
log_msg("Deleted %i rows in %0.3fs.\n\n",n,this.query.time[3])

#'========================================================================
# Get lists of data to process ####
#'========================================================================
log_msg("Getting list of data to process...\n")

# Calibrated data -----------------------------------------------------------------

#Get calibration methods
these.calibs <- 
  if(length(this.stat@calibration)==0) {
  pcfg@calibrationMethods
}  else {
  this.stat@calibration} 
  
#SQL table
cal.SQL <-  #Calibrations x realisations frags
  expand_grid(calibration=these.calibs,
              sources=this.stat@sources) %>%  
  filter(sources!=0) %>% #Handled 0 separately
  mutate(WHERE.real=
           case_when(sources==1 ~ "`srcType`='Observations'",
                     sources==2 ~ paste("NOT(`realization` IN ('realmean', 'ensmean')) AND",
                                             "NOT(`srcType`= 'Observations')"),
                     sources==3 ~ "`realization` = 'realmean'",
                     sources==4 ~ "`realization` = 'ensmean'",
                     TRUE~ as.character(NA))) %>%
  mutate(SQL.cmd=sprintf("SELECT pKey FROM %s WHERE `calibrationMethod` LIKE '%s%%' AND %s",
                         PE.cfg$db$calibration,
                         calibration,
                         WHERE.real))

#Now get list of pKeys to process
this.query.time <- 
  system.time({
    cal.SQL <- 
      cal.SQL %>%
      mutate(pKeys=map(SQL.cmd,~ PE.db.getQuery(pcfg,PE.cfg$db$calibration,.x)),
             nKeys=map_dbl(pKeys,nrow)) 
  })
log_msg("Complete in %0.3fs.\n",this.query.time[3])

#And generate a todo list
cal.pKeys <- 
  cal.SQL %>%
  unnest(pKeys) %>% 
  pull(pKey)
dmp <- assert_that(!any(duplicated(cal.pKeys)),msg="Expecting unique set of pKeys to process")

# Uncalibrated observations -------------------------------------------------------
if(any(this.stat@sources==0)) {  #Have to explicitly ask for it
  tb.extr <- PE.db.tbl(pcfg,PE.cfg$db$extr)
  obs.sel <- 
    tb.extr %>%
    filter(srcType=="Observations") %>%
    select(pKey,date) %>%
    collect() %>%
    #Filter by month of interest
    mutate(date=ymd(date),
           month=month(date)) %>%
    filter(month %in% pcfg@MOI)
  dbDisconnect(tb.extr)
  
  obs.pKeys <- 
    obs.sel %>%
    pull(pKey)
} else  {
  obs.pKeys <- NULL
}

# Combine into a todo list --------------------------------------------------------
todo.tbl <- 
  bind_rows(tibble(pKey=cal.pKeys,
                   tbl=PE.cfg$db$calibration),
            tibble(pKey=obs.pKeys,
                   tbl=PE.cfg$db$extract))
dmp <- assert_that(nrow(todo.tbl)!=0,
                   msg="Nothing to do!")

#'========================================================================
# Calculation of statistics ####
#'========================================================================
#Setup landmask 
if(length(pcfg@landmask)==0) {
  #Create a simple raster that includes everything
  landmask <- raster(pcfg@global.ROI)
  res(landmask) <- pcfg@global.res
  landmask[] <- 0   #Include everything
} else {
  #Use the filename to specify and remap
  landmask.cmd <- cdo("--silent -f nc",
                      csl(" remapnn", PE.scratch.path(pcfg,"analysis.grid")),
                      pcfg@landmask,
                      PE.scratch.path(pcfg,"landmask"))
  landmask <- raster(PE.scratch.path(pcfg,"landmask")) 
}

#Setup the mask for the corresponding spatial boundary
#based on the combination of the landmask and the spatial boundary mask
combined.mask <- mask(landmask,as(this.sp,"Spatial"),updatevalue=1)

# Stat calculation function -------------------------------------------------------
calc.stat.fn <- function(this.pKey,this.table) {
  #Debugging
  # this.pKey <- cal.pKeys[1]
  # this.table <- PE.cfg$db$calibration
  # this.pKey <- obs.pKeys[1]
  # this.table <- PE.cfg$db$extract
  
  #Import data
  this.dat <- 
    sprintf("SELECT * FROM %s WHERE `pKey` = %i",
            this.table,
            this.pKey) %>%
    PE.db.getQuery(pcfg,this.table,this.sql = .) %>%
    as_tibble() %>%
    select(-any_of(c("pKey","srcFname"))) %>%
    PE.db.unserialize() 
  
  #Apply the masks to data
  masked.dat <- mask(this.dat$field[[1]],combined.mask,maskvalue=1)
  
  #And we're ready. Lets calculate some statistics
  this.res <- eval.stat(st=this.stat,dat=masked.dat) 
  
  #Store the results
  out.dat <-
    this.dat %>%
    select(!field) %>% #Drop the source data
    add_column(spName=sp.id,
               statName=stat.id) %>%
    bind_cols(this.res) 
  
  return(out.dat)
}

# Sanity check --------------------------------------------------------------------
# Try doing the first evaluation as a sanity check. This will let us fail gracefully,
# before getting medieval on their asses...
log_msg("Dry run....\n")
dmp.cal <- 
  todo.tbl %>%
  head(1) %>%
  mutate(map2(pKey,tbl,calc.stat.fn))
log_msg("Sanity check passed. Parallellising using %i cores...\n",n.cores)


# Chunking ------------------------------------------------------------------------
chunk.l <-
  todo.tbl %>%
  mutate(batch.id=rep(seq(nrow(.)),
                    each=n.cores*5,
                    length.out=nrow(.))) %>%
  group_by(batch.id) %>%
  group_split()

#Progress bar
pb <- PE.progress(length(chunk.l))
dmp <- pb$tick(0)

# Parallelised extraction loop ----------------------------------------------------
for(this.chunk in chunk.l) {
  #Using furrr
  stat.dat <-
    future_map2_dfr(this.chunk$pKey,
                    this.chunk$tbl,
                    calc.stat.fn, 
                    .options = furrr_options(stdout=FALSE,
                                             seed=TRUE))
  #Write results
  PE.db.appendTable(pcfg,PE.cfg$db$stats,stat.dat)
  
  #Loop
  pb$tick()
}

#'========================================================================
# Complete ####
#'========================================================================
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
