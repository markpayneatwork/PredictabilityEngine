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
})
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(!exists("...")) {  #Then we are running as a script
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  sel.cfg <- "NorCPM.i2"
  this.datasrc <- pcfg@Decadal[[sel.cfg]]
} else { #Running as a function
  #Inputs are supplied as named arguments to the function version
  args.in <- list(...)
  assert_that(!is.null(args.in$this.datasrc),msg="'this.datasrc' not supplied as argument")
  this.datasrc <- args.in$this.datasrc
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
}

log_msg("Performing analysis for: %s, %s", this.datasrc@type, this.datasrc@name)


#'========================================================================
# Clear existing realmeans ####
#'========================================================================
#Setup query
SQL.cmd <- sprintf("SELECT pKey FROM %s WHERE `srcType` = '%s' AND `srcName` = '%s' AND `realization` = 'realmean'",
                   PE.cfg$db$extract,
                   this.datasrc@type,
                   this.datasrc@name)
#Fetch list to delete
del.these <- PE.db.getQuery(pcfg,SQL.cmd,silent=FALSE)

#Delete
PE.db.delete.by.pKey(pcfg,PE.cfg$db$extract,del.these$pKey,silent=TRUE)

#'========================================================================
# Retrieve data ####
#'========================================================================
#Retrieve everything relevant at once. We might go chunkwise in the future, but this
#will do to start with

#Extract data and perform averaging
SQL.cmd <- sprintf("SELECT * FROM %s WHERE `srcType` = '%s' AND `srcName`= '%s'",
                   PE.cfg$db$extract,
                   this.datasrc@type,
                   this.datasrc@name)
frag.dat <- 
  PE.db.getQuery(pcfg,SQL.cmd,silent=TRUE) %>%
  PE.db.unserialize()

#'========================================================================
# Calculate  ####
#'========================================================================
#Calculate realmeans
realMeans <- 
  frag.dat %>%
  group_by(srcType,srcName,startDate,date,leadIdx,.drop=TRUE) %>%
  summarise(data=raster.list.mean(data),
            duplicate.realizations=any(duplicated(realization)),
            .groups="keep") %>% #Check for duplicated realization codes
  ungroup()
if(any(realMeans$duplicate.realizations)) stop("Duplicate realizations detected in database. Rebuild.")

#Write to database 
realMeans %>%
  select(-duplicate.realizations) %>%
  add_column(realization="realmean",.after="srcType") %>%
  add_column(srcHash=as.character(NA),.before=1) %>%
  PE.db.appendTable(pcfg, PE.cfg$db$extract)

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
