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
  cfg.id <- 1
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
this.cfg <- todo.list[cfg.id,]

#Extract elements based on configuration
this.stat <- this.cfg$stat.obj[[1]]
this.sd <- this.cfg$sd.geometry[[1]]
log_msg("Stat   : %s\nCalib  : %s\nReal   : %i\nSp.Dom : %s\n",
        this.stat@name,this.cfg$stat.calibration,this.cfg$stat.realizations,
        this.cfg$sd.name)

#Load of fragments to process 
#We have chosen here to load it all into memory, as it simplifies the process
#and avoids generating issues with concurrency (I hope). However, if this
#gets to be too big in the future, we may need to rethink the approach. 
#We need to choose here between Observation and model types
select.by.realization.code <- function(realization.code) {
  switch(as.character(realization.code),
         "1"="NOT(`srcType` IN ('realmean', 'ensmean'))",
         "2"="`realization` = 'realmean'",
         "3"="`srcName` = 'ensmean'",
         stop("Unknown option"))
    
}
if(this.cfg$stat.realizations==0) {
  #Use observations
  frag.src <- PE.cfg$db$extract
  frag.todo.SQL <- 
    sprintf("SELECT pKey FROM %s WHERE (`srcType` = 'Observations')",
            frag.src)
} else {
  #Use calibrated model outputs
  frag.src <- PE.cfg$db$calibration
  frag.todo.SQL <- 
    sprintf("SELECT pKey FROM %s WHERE %s AND `calibrationMethod` = '%s'",
            frag.src,
            select.by.realization.code(this.cfg$stat.realizations),
            this.cfg$stat.calibration)
}  

#Get list of ids to process
ids.todo <-
  PE.db.getQuery(pcfg,frag.todo.SQL) %>%
  pull() 

#Clear existing results 
existing.stats.sel <- 
  sprintf("SELECT pKey FROM %s WHERE `statName` = '%s' AND `sdName` = '%s'",
          PE.cfg$db$stats,
          this.cfg$stat.name,
          this.cfg$sd.name)
if(this.cfg$stat.realizations==0) {
  existing.stats.sel <-
    sprintf("%s AND `srcType` = 'Observations'",existing.stats.sel)

} else {
  existing.stats.sel <-
    sprintf("%s AND %s AND `calibrationMethod` = '%s'",
            existing.stats.sel,
            select.by.realization.code(this.cfg$stat.realizations),
            this.cfg$stat.calibration)
}
del.these <- 
  PE.db.getQuery(pcfg,existing.stats.sel) %>%
  pull()
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
    sprintf("SELECT * FROM %s WHERE `pKey` = %i",
            frag.src,
            i) %>%
    PE.db.getQuery(pcfg=pcfg) %>%
    as_tibble() %>%
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
    select(-data,-any_of("srcHash"))
  
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
