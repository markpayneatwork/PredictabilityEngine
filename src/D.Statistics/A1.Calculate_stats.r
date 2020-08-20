#'========================================================================
# A1.Calculate_stats
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
cat(sprintf("\n%s\n","A1.Calculate_stats"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

#Helper functions, externals and libraries
library(PredEng)
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  debug.mode <- TRUE
  set.log_msg.silent()
} else {
  debug.mode <- FALSE
}

#'========================================================================
# Setup the todo list ####
#'========================================================================
#Setup databases
this.db <- PE.db.connection(pcfg)
calib.tbl <- tbl(this.db,PE.cfg$db$calibration)

#Get the observational references
frag.tbl  <- tbl(this.db,PE.cfg$db$extract)

#Reset the resutls table by deleting and reestablishing it
dbRemoveTable(this.db,PE.cfg$db$stats)
PE.db.setup(pcfg)

#Setup landmask 
landmask <- raster(PE.scratch.path(pcfg,"landmask"))

# The configuration of the statistics informs many aspects of the analysis, including
# the spatial domain that we need to apply. We therefore extract the metadata for the stats
stat.obj.meta <- 
  tibble(stat.name=map_chr(pcfg@statistics ,slot,name="name"),
         stat.use.spatial.polygon=map_lgl(pcfg@statistics,slot,name="use.spatial.polygons"),
         stat.obj=map(pcfg@statistics,~.x),
         stat.returns.field=map_lgl(pcfg@statistics,returns.field),
         stat.realizations=map(pcfg@statistics,slot,name="realizations"),
         stat.calibration=map(pcfg@statistics,slot,"calibration"))

#Duplicate the stats to account for observations
stat.obj.obs <-
  stat.obj.meta %>%
  mutate(stat.realizations=list(0),
         stat.calibration=list(NA))

#Combine and expand them to give the list of stat jobs to do
stat.jobs <- 
  bind_rows(stat.obj.meta,stat.obj.obs) %>%
  unnest(stat.calibration) %>%   #Automatically handles the combinatorial aspect
  unnest(stat.realizations) %>%
  arrange(stat.name,stat.realizations) %>%
  add_column(statJob.id=seq(nrow(.)),.before=1)

#Develop a similar table for the spatial domains
sd.tb <- 
  pcfg@spatial.polygons %>%
  mutate(sd.is.spatial.polygon=TRUE) %>%
  rename(sd.name=name,sd.geometry=geometry) %>%
  rbind(st_sf(sd.geometry=st_sfc(sfpolygon.from.extent(pcfg@global.ROI)),
              sd.name="GlobalROI",
              sd.is.spatial.polygon=FALSE)) %>%
  add_column(sd.id=1:nrow(.),.before=1)

# Merge everything into one large overview table of what needs to be done
# We then restrict the full set of cominbations to combinations that are
# requested / sane
todo.list <- 
  expand_grid(sd.id=sd.tb$sd.id,
              statJob.id=stat.jobs$statJob.id) %>%
  #Add in metadata
  left_join(y=stat.jobs,by="statJob.id") %>%
  left_join(y=sd.tb,by="sd.id") %>%
  #Drop combinations that don't make sense
  #  - ensure match between needs for spatial vs global polygons
  filter(sd.is.spatial.polygon==stat.use.spatial.polygon) %>%
  #  - remove duplicate
  #Filter
  select(-sd.id,-statJob.id)

#'========================================================================
# Calculation of statistics ####
#'========================================================================
for(j in seq(nrow(todo.list))) {
  #Extract elements based on configuration
  this.cfg <- todo.list[j,]
  this.stat <- this.cfg$stat.obj[[1]]
  this.sd <- this.cfg$sd.geometry[[1]]
  log_msg("Stat: '%s', Calib: '%s', Real: %i, Sp.Dom: '%s'... (%i of %i)\n",
          this.stat@name,this.cfg$stat.calibration,this.cfg$stat.realizations,
          this.cfg$sd.name,j,nrow(todo.list))
  
  #Setup the mask for the corresponding spatial boundary
  #based on the combination of the landmask and the spatial boundary mask
  combined.mask <- mask(landmask,as(this.sd,"Spatial"),updatevalue=1)
  
  #Get list of calibration fragments to process 
  #We choose here between Observation and model types
  if(this.cfg$stat.realizations==0) {
    frags.todo <- filter(frag.tbl,srcType=="Observations")
  } else {
    frags.todo <- 
      switch(as.character(this.cfg$stat.realizations),
             "1"=filter(calib.tbl,!realization %in% c("realmean","ensmean")),
             "2"=filter(calib.tbl,realization=="realmean"),
             "3"=filter(calib.tbl,realization=="ensmean"),
             stop("Unknown option")) %>%
      filter(calibrationMethod == !!this.cfg$stat.calibration) 
  }  
  
  #Get list of ids to process
  ids.todo <-
    frags.todo %>%
    select(pKey) %>%
    collect() %>%
    pull() 

  #Then loop over fragments
  pb <- PE.progress(ids.todo)
  pb$tick(0)
  for(i in ids.todo) {
    
    #Import data
    this.dat <- 
      frags.todo %>%
      filter(pKey==i) %>%
      collect() %>%
      PE.db.unserialize() %>%
      select(-pKey)
    
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
    PE.db.appendTable(out.dat,this.db,PE.cfg$db$stats)
    
    #Loop back
    pb$tick()
  }  #/end loop over calibrated fragments

} #/end loop over configs




#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

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
