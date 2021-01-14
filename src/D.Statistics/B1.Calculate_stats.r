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
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.log_msg.silent()
  stat.id <- names(pcfg@statistics)[2]
  sp.id <- c(PE.cfg$misc$globalROI,pcfg@spatial.polygons$name)[2]
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
dmp <- assert_that(all(sp.id %in% c(PE.cfg$misc$globalROI,pcfg@spatial.polygons$name)) & length(sp.id)==1,
            msg="Unknown spatial polygon(s) requested")

#Extract elements to process
this.stat <- pcfg@statistics[[stat.id]]
this.sp <- 
  if(sp.id == PE.cfg$misc$globalROI) {
    assert_that(this.stat@use.globalROI,msg="Function requested globalROI, but stat doesn't use one.")
    PE.global.sf(pcfg) 
  } else {
    assert_that(!this.stat@use.globalROI,msg="Function didn't request globalROI, but stat wants one.")
    pcfg@spatial.polygons %>% 
      filter(name==sp.id)
  }
dmp <- assert_that(nrow(this.sp)==1,msg="Failed to select only one spatial polygon")

#TODO:
# Parallelise, reduce write frequency

#Display Configuration
PE.config.summary(pcfg,
                  "spName"=sp.id,
                  "statName"=stat.id,
                  "calibrationMethod(s)"=paste0(this.stat@calibration,collapse=", "),
                  "realizations"=paste0(this.stat@realizations,collapse=", "))

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
      PE.db.getQuery(pcfg,existing.stats.sel) %>%
      pull()
  })
log_msg("Complete in %0.3fs. \nDeleting...",this.query.time[3])
this.query.time <- 
  system.time({
    n <- PE.db.delete.by.pKey(pcfg=pcfg,tbl.name=PE.cfg$db$stats,pKeys = del.these.pKeys)
  })
log_msg("Deleted %i rows in %0.3fs.\n\n",n,this.query.time[3])

#'========================================================================
# Get lists of fragments to process ####
#'========================================================================
log_msg("Getting list of calibrations to process...\n")
#Processing frags
cr.frags <-  #Calibrations x realisations frags
  expand_grid(calibration=if(length(this.stat@calibration)==0) pcfg@calibrationMethods else this.stat@calibration,
              realizations=this.stat@realizations) %>%  
  mutate(real.SQL.sel=case_when(
    realizations==1 ~ "`srcType`='Observations'",
    realizations==2 ~ "NOT(`realization` IN ('realmean', 'ensmean')) AND NOT(`srcType`= 'Observations')",
    realizations==3 ~ "`realization` = 'realmean'",
    realizations==4 ~ "`realization` = 'ensmean'",
    TRUE~ as.character(NA))) %>%
  mutate(SQL.sel=sprintf("SELECT pKey FROM %s WHERE `calibrationMethod` LIKE '%s%%' AND %s",
                         PE.cfg$db$calibration,
                         calibration,
                         real.SQL.sel))

#Now get list of pKeys to process
this.query.time <- 
  system.time({
    todo.frags <- 
      cr.frags%>%
      mutate(pKeys=map(SQL.sel,~ PE.db.getQuery(pcfg,.x)),
             nKeys=map_dbl(pKeys,nrow))
  })
log_msg("Complete in %0.3fs.\n",this.query.time[3])

#And generate a todo list
pKey.todos <- 
  todo.frags %>%
  unnest(pKeys) %>% 
  pull(pKey)
dmp <- assert_that(!any(duplicated(pKey.todos)),msg="Expecting unique set of pKeys to process")

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
calc.stat.fn <- function(this.pKey) {
  #Debugging
  # this.pKey <- pKey.todos[1]

  #Import data
  this.dat <- 
    sprintf("SELECT * FROM %s WHERE `pKey` = %i",
            PE.cfg$db$calibration,
            this.pKey) %>%
    PE.db.getQuery(pcfg=pcfg) %>%
    as_tibble() %>%
    select(-pKey) %>%
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
dmp <- map(head(pKey.todos,1),calc.stat.fn)
log_msg("Sanity check passed. Parallellising using %i cores...\n",n.cores)

# Chunking ------------------------------------------------------------------------
n.todo <- length(pKey.todos)
chunk.l <- 
  split(pKey.todos,
        rep(seq(n.todo),  
            each=n.cores*20,
            length.out=n.todo))

#Progress bar
pb <- PE.progress(length(chunk.l))
dmp <- pb$tick(0)

# Parallelised extraction loop ----------------------------------------------------
for(this.chunk in chunk.l) {
  #Using furrr
  stat.dat <-
    future_map_dfr(this.chunk,
                   calc.stat.fn, 
                   .options = furrr_options(stdout=FALSE,
                                            seed=TRUE))
  
  #Write results
  PE.db.appendTable(stat.dat,pcfg,PE.cfg$db$stats)
  
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
