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
# Calculates the individual statistics
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
})
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.log_msg.silent()
  cfg.id <- 4
} else {
  cmd.args <- commandArgs(TRUE)
  if(length(cmd.args)!=1) stop("Cannot get command args")
  cfg.id <- as.numeric(cmd.args)
  set.log_msg.silent()
}

log_msg("Configuration\nCfg id : %i\n",cfg.id)

#'========================================================================
# Setup  ####
#'========================================================================
#Setup item todo
todo.list <- readRDS(PE.scratch.path(pcfg,"statjoblist"))
this.cfg <- todo.list[[cfg.id]]

#Extract elements based on configuration
this.stat <- this.cfg$stat.obj[[1]]
this.sd <- this.cfg$sd.geometry[[1]]
log_msg("Stat   : %s\nCalib  : %s\nReal   : %i\nSp.Dom : %s\n",
        this.stat@name,this.cfg$stat.calibration,this.cfg$stat.realizations,
        this.cfg$sd.name)

#Setup databases
this.db <- PE.db.connection(pcfg)
calib.tbl <- tbl(this.db,PE.cfg$db$calibration)
extr.tbl  <- tbl(this.db,PE.cfg$db$extract) #Get the observational references
stats.tbl <- tbl(this.db,PE.cfg$db$stats) 

#Load of fragments to process 
#We have chosen here to load it all into memory, as it simplifies the process
#and avoids generating issues with concurrency (I hope). However, if this
#gets to be too big in the future, we may need to rethink the approach. 
#We need to choose here between Observation and model types
filter.by.realization.code <- function(tb,realization.code) {
  switch(as.character(realization.code),
         "1"=filter(tb,!realization %in% c("realmean","ensmean")),
         "2"=filter(tb,realization=="realmean"),
         "3"=filter(tb,srcName=="ensmean"),
         stop("Unknown option"))
    
}
if(this.cfg$stat.realizations==0) {
  #Use observations
  frags.todo <- 
    extr.tbl %>% 
    filter(srcType=="Observations") %>%
    collect()
} else {
  #Use calibrated model outputs
  frags.todo <- 
    calib.tbl %>% 
    filter.by.realization.code(this.cfg$stat.realizations) %>%
    filter(calibrationMethod == !!this.cfg$stat.calibration) %>%
    collect()
}  

#Get list of ids to process
ids.todo <-
  frags.todo %>%
  select(pKey)%>%
  pull() 

#Clear existing results 
existing.stats <-
  stats.tbl %>%
  filter(statName==!!this.stat@name,
         sdName==!!this.cfg$sd.name) 
if(this.cfg$stat.realizations==0) {
  existing.stats <-
    existing.stats %>%
    filter(srcType=="Observations")
} else {
  existing.stats <-
    existing.stats %>%
    filter.by.realization.code(this.cfg$stat.realizations) %>%
    filter(calibrationMethod==!!this.cfg$stat.calibration)
}
del.these <- 
  existing.stats %>%
  select(pKey) %>%
  collect() %>%
  pull(pKey)
dbDisconnect(this.db)
PE.db.delete.by.pKey(pcfg=pcfg,tbl.name=PE.cfg$db$stats,pKeys = del.these)

#'========================================================================
# Calculation of statistics ####
#'========================================================================
#Setup landmask 
landmask <- raster(PE.scratch.path(pcfg,"landmask"))

#Setup the mask for the corresponding spatial boundary
#based on the combination of the landmask and the spatial boundary mask
combined.mask <- mask(landmask,as(this.sd,"Spatial"),updatevalue=1)

#Then loop over fragments
pb <- PE.progress(ids.todo)
dmp <- pb$tick(0)
for(i in ids.todo) {
  #Import data
  this.dat <- 
    frags.todo %>%
    filter(pKey==i) %>%
    select(-pKey) %>%
    PE.db.unserialize() 

  #Apply the masks to data
  masked.dat <- mask(this.dat$data[[1]],combined.mask,maskvalue=1)
  
  #And we're ready. Lets calculate some statistics
  this.res <- eval.stat(st=this.stat,dat=masked.dat) 
  
  #Store the results
  out.dat <-
    this.dat %>%
    add_column(sdName=this.cfg$sd.name,
               statName=this.stat@name) %>%
    bind_cols(this.res) %>%
    select(-data) 
  PE.db.appendTable(out.dat,pcfg,PE.cfg$db$stats)
  #Loop back
  dmp <- pb$tick()
}  #/end loop over calibrated fragments

#'========================================================================
# Complete ####
#'========================================================================
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
