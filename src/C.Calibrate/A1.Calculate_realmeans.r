#'========================================================================
# Calculate_realmeans
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed Sep  2 16:30:47 2020
#
# Calculates the realisation means for a given input datasource
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# Script arguments
#    this.datasrc
#
# To do:
#
# Notes:
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Calculate_realmeans"))
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
  this.srcType <- "Decadal"
  this.srcName <- "CESM.DPLE"
} else {  #Running as a "function"
  cmd.args <- commandArgs(TRUE)
  assert_that(length(cmd.args)==2,msg="Cannot get command args")
  this.srcType <- cmd.args[1]
  this.srcName <- cmd.args[2]
}
set.cdo.defaults("--silent --no_warnings -O")
set.log_msg.silent()

this.datasrc <- PE.get.datasrc(pcfg,this.srcType,this.srcName)
PE.config.summary(pcfg,this.datasrc)

#Setup parallelisation
if(Sys.info()["nodename"]=="aqua-cb-mpay18" | interactive()) {
  n.cores <- 8
} else if(Sys.info()["nodename"]=="volta.dmi.dk" ) {
  n.cores <- 8
} else {
  n.cores <- as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))    
  assert_that(!is.na(n.cores),msg = "Cannot detect number of allocated cores")
}
plan(multisession,workers = n.cores)

#'========================================================================
# Clear existing realmeans ####
#'========================================================================
#Setup query
SQL.cmd <- sprintf("SELECT pKey FROM %s WHERE `srcType` = '%s' AND `srcName` = '%s' AND `realization` = 'realmean'",
                   PE.cfg$db$extract,
                   this.datasrc@type,
                   this.datasrc@name)

#Fetch list to delete
del.these <- PE.db.getQuery(pcfg,PE.cfg$db$extract,this.datasrc,SQL.cmd)

#Delete
PE.db.delete.by.pKey(pcfg,PE.cfg$db$extract,this.datasrc,del.these$pKey,silent=FALSE)

#'========================================================================
# Calculate means ####
#'========================================================================
#Setup database
this.db <- PE.db.connection(pcfg,PE.cfg$db$extract,this.datasrc)
extr.tbl <- tbl(this.db,PE.cfg$db$extract)

#Get meta data
chunk.meta <-
  extr.tbl %>%
  filter(srcType==!!this.datasrc@type,
         srcName==!!this.datasrc@name) %>%
  select(pKey,srcType,srcName,realization,startDate,date,lead) %>%
  collect() %>%
  group_by(across(-c(pKey,realization)),.drop=TRUE) %>%
  mutate(grp.idx=cur_group_id())

#Divide into baskets
log_msg("Calculate realisation means using %i coress...\n",n.cores)
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
    extr.tbl %>%
    filter(pKey %in% these.pKeys) %>%
    collect() %>%
    PE.db.unserialize() %>%
    #Nest into chunks
    nest(field.tbl=c(pKey,srcFname,realization,field)) %>%
    mutate(field.l=map(field.tbl,pull,"field")) 
  
  #Process in parallel
  realMeans <-
    these.chunks %>%
    mutate(field=future_map(field.l,
                            ~ mean(brick(.x)),
                            .options = furrr_options(stdout=FALSE,
                                                     seed=TRUE)),
           
           realization="realmean") %>%
    mutate(srcFname=as.character(NA)) %>%
    select(-field.l,-field.tbl)

  #Write to database 
  realMeans %>%
    PE.db.appendTable(pcfg, PE.cfg$db$extract,this.datasrc,dat=.)

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
