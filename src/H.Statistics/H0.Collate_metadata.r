#'========================================================================
# Collate metadata
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Oct 17 20:23:38 2019
#
# Collates the metadata for all potential input files and the takes care 
# of distributing them across tasks manually. This approach is intended to
# be adaptive in nature, so that it is robust to the availability or lack therefore
# of the individual files. This is important when space limitations start 
# to kick in, as not all of the result files need to be  present or
# processed for this to work. 
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
cat(sprintf("\n%s\n","Collate metadata"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
library(PredEng)
library(ncdf4)
pcfg <- readRDS(PE.cfg$config.path)

#'========================================================================
# Configure ####
#'========================================================================
#No input arguments

#'========================================================================
# Setup ####
#'========================================================================
#Retrieve the combination of data sets, sources and spatial configs
src.tb <-  
  partition.workload(pcfg) %>%
  rename(src.id=cfg.id)

# The basic unit of this analysis is defined as follows:
# * spatial area
# * statistic
# * data source (whoose type can be influenced by the statistics)
# For simplicity, lets call this a statistical atom. We define the partitioning
# across these atoms as the basic, indivisible unit.

#The statistics, in principle, determine the type of data that should be used
# as an input (i.e. realmeans, realizations etc) and thereby the metadata that
# we want to collate. They  can also inform the spatial domain that we are interested in
#as well - particularly for statistics that work with  fields, instead of singular
#values, we want to process the entire global domain, rather than just a single
#local spatial domain. Hence, we need to select the spatial statistics accordingly.
stats.tb <- tibble(stat.name=map_chr(pcfg@statistics ,slot,name="name"),
                   stat.use.globally=map_lgl(pcfg@statistics,slot,name="use.globally"),
                   stat.obj=pcfg@statistics,
                   stat.returns.field=map_lgl(pcfg@statistics,returns.field),
                   stat.uses.realmeans=map_lgl(pcfg@statistics,slot,name="use.realmeans"),
                   stat.id=seq(stat.name))

#Develop a similar table for the spatial aspects
sp.tb <- tibble(sp.obj=c(global.ROI(pcfg),pcfg@spatial.domains),
                sp.name=map_chr(sp.obj,slot,name="name"),
                sp.is.global=sp.name==PE.cfg$misc$global.sp.name,
                sp.id=seq(sp.obj))

#Merge everything into one large overview table of what needs to be done
todo.tbl <- 
  expand.grid(src.id=src.tb$src.id,
              sp.id=sp.tb$sp.id,
              stat.id=stats.tb$stat.id) %>%
  as_tibble() %>%
  #Add in metadata
  left_join(y=src.tb,by="src.id") %>%
  left_join(y=stats.tb,by="stat.id") %>%
  left_join(y=sp.tb,by="sp.id") %>%
  #Drop combinations that don't make sense
  #  - ensemble means using individual realisations 
  filter(!(src.name==PE.cfg$files$ensmean.name & !stat.uses.realmeans)) %>%
  #  - field statistics using local domains
  filter(sp.is.global==stat.use.globally) %>%
  #Create output file name (one file per atom)
  mutate(stat.fname=sprintf("%s_%s_%s_%s.rds",src.type,src.name,sp.name,stat.name))

#Now use nesting trick to keep the total number of jobs under 1024, which is
#the maximum number of items that a job array can handle in LSB
max.jobs <- 1024
if(nrow(todo.tbl)>max.jobs) {
  #Nest first the stats
  do.this <- nest(todo.tbl,-src.id,-sp.id,.key="nested")
  if(nrow(do.this)> max.jobs) {
    #Still too big, so nest the stats and sp
    do.this <- nest(todo.tbl,-src.id,.key="nested")
  }
} else {
  do.this <- todo.tbl
}

if(nrow(do.this)>max.jobs) stop("Unable to nest todo list sufficiently.")
do.this$cfg.id <- seq(nrow(do.this))

#'========================================================================
# Output and setup ####
#'========================================================================
#Write configuration data
do.this %>%
  select(cfg.id) %>%
  write_csv(path = file.path(PE.cfg$dirs$job.cfg,"Stats.cfg"))
saveRDS(do.this,file.path(PE.cfg$dirs$job.cfg,"Stats.rds"))
stat.cfg.dir <-file.path(PE.cfg$dirs$job.cfg,"Stats") 
if(!dir.exists(stat.cfg.dir)) dir.create(stat.cfg.dir)

#Clear the existing statistics, to avoid incomplete duplication etc
#Directory setup
stat.dir <- define_dir(pcfg@scratch.dir,PE.cfg$dirs$statistics)
unlink(dir(stat.dir,full.names=TRUE))

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
