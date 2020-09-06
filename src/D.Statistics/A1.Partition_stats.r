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
# Partitions the workload for calculating individual statistics
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
cat(sprintf("\n%s\n","A1.Partition_stats"))
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
  debug.mode <- TRUE
  set.log_msg.silent()
} else {
  debug.mode <- FALSE
}

#'========================================================================
# Setup the todo list ####
#'========================================================================
# The configuration of the statistics informs many aspects of the analysis, including
# the spatial polygon that we need to apply. We therefore extract the metadata for the stats
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

#Develop a similar table for the spatial polygons
sp.tb <- 
  pcfg@spatial.polygons %>%
  mutate(sp.is.spatial.polygon=TRUE) %>%
  rename(sp.name=name,sp.geometry=geometry) %>%
  rbind(st_sf(sp.geometry=st_sfc(sfpolygon.from.extent(pcfg@global.ROI)),
              sp.name="GlobalROI",
              sp.is.spatial.polygon=FALSE,
              crs=crs(pcfg@spatial.polygons))) %>%
  add_column(sp.id=1:nrow(.),.before=1)

# Merge everything into one large overview table of what needs to be done
# We then restrict the full set of cominbations to combinations that are
# requested / sane
todo.list <- 
  expand_grid(sp.id=sp.tb$sp.id,
              statJob.id=stat.jobs$statJob.id) %>%
  #Add in metadata
  left_join(y=stat.jobs,by="statJob.id") %>%
  left_join(y=sp.tb,by="sp.id") %>%
  #Drop combinations that don't make sense
  #  - ensure match between needs for spatial vs global polygons
  filter(sp.is.spatial.polygon==stat.use.spatial.polygon) %>%
  #  - remove duplicate
  #Set ids
  select(-sp.id,-statJob.id) 

todo.list %>%
  saveRDS(file=PE.scratch.path(pcfg,"statjoblist"))

#Clear all stats results
dbRemoveTable(PE.db.connection(pcfg),PE.cfg$db$stats)
PE.db.setup(pcfg)

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
