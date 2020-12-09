#'========================================================================
# A1.Pointwise_extraction
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed Dec  9 06:23:26 2020
#
# Extracts data from the database by interpolating at a specific point in 
# space and time. 
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
cat(sprintf("\n%s\n","A1.Pointwise_extraction"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressMessages({
  library(PredEng)
  library(pbapply)
#  library(furrr)
})
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.log_msg.silent()
  n.cores <- 4
 # plan(multicore(workers=4)) 
  pboptions(type="txt")
} else {
  cmd.args <- commandArgs(TRUE)
  if(length(cmd.args)!=1) stop("Cannot get command args")
  #plan(multicore(workers=cmd.args[1])) 
  n.cores <- cmd.args[1]
  set.log_msg.silent()
  pboptions(type="none")
}

#'========================================================================
# And Go ####
#'========================================================================
#Loop over rows in pointwise extraction
extr.res.l <- list()
for(i in seq(nrow(pcfg@pt.extraction))) {
  #Setup data extraction
  this.row <- pcfg@pt.extraction[i,]
  this.db <- PE.db.connection(pcfg,results.db = pcfg@pt.extraction.from.results.db)
  this.tb <- 
    tbl(this.db,this.row$table) 
  this.meta <- 
    this.row %>%
    select(-points)

  #Now get list of dates available that meet the filter
  filt.dat <- 
    this.tb %>%
    filter(rlang::parse_expr(this.row$filter)) %>%
    select(pKey,date) %>%
    collect() %>%
    mutate(ym=date_to_ym(date))
  
  #Now prepare the points
  pt.sf <- 
    this.row$points[[1]] %>%
    rename(extr.date=date) %>%   #To avoid conflicts with data date field
    mutate(ym=date_to_ym(extr.date)) %>%
    group_by(ym) 
  
  
  #Extraction function
  #Temporal matching is done by ym code initially.
  pt.extractor <- function(this.sf) {
    # #Debug setup
    # this.sf <- group_split(pt.sf)[[1]]

    #Extract splitting key manually
    this.key <- unique(this.sf$ym)
    
    #Find data that we need to extract
    these.pKeys <-
      filt.dat %>%
      filter(ym==this.key) %>% 
      pull(pKey)
    this.dat <-
      this.tb %>%
      filter(pKey %in% these.pKeys) %>%
      collect() %>%
      PE.db.unserialize()
    
    #Now apply the extraction algorithm
    this.extr <-
      this.dat %>%
      mutate(extract.df=map(data,~cbind(this.meta,
                                        st_coordinates(this.sf),
                                        st_drop_geometry(this.sf),
                                        extraction=raster::extract(.x,
                                                                   this.sf,
                                                                   method="bilinear"))))
    #Drop data and return
    this.extr %>%
      select(-data) %>%
      return()
  }
  
  #Apply function
  pt.st.l <- group_split(pt.sf)
  extr.res.l[[i]] <- pblapply(group_split(pt.sf),
                              pt.extractor,
                              cl = 1)
  # extr.res.l <-
  #   group_map(pt.sf,~pt.extractor(.x,.y))
}

extr.res <-
  extr.res.l %>%
  bind_rows() %>% 
  relocate(extract.df) %>%
  unnest(extract.df)

#'========================================================================
# Output ####
#'========================================================================
#Clear existing extractions table
if(dbExistsTable(PE.db.connection(pcfg),PE.cfg$db$pt.extraction)) {
  dbRemoveTable(PE.db.connection(pcfg),PE.cfg$db$pt.extraction)  
}

extr.res %>%
  PE.db.appendTable(pcfg,PE.cfg$db$pt.extraction)

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
dbDisconnect(this.db)

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
