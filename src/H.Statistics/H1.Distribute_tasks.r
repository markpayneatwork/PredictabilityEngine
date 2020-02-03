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

# The statistics can inform the spatial domain that we need to apply -
# particularly for statistics that work with  fields, instead of singular
# values, we want to process the entire global domain, rather than just a single
# local spatial domain. Hence, we need to ensure that there is a match between
# these two aspects. We therefore extract the metadata for the stats
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

# Merge everything into one large overview table of what needs to be done
# We then restrict the full set of cominbations, by ensuring that there is
# agreement between the stats to be calculated and the spatial subdomain
# Also need agreement between the metadata being loaded in this configuration
# and the distinction between realisations/realmeans

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
  #Select metadata source - this is determined by the configurationof the stat
  mutate(metadat.fname=ifelse(stat.uses.realmeans,PE.cfg$files$realmean.meta,PE.cfg$files$anom.meta),
         metadat.path=file.path(pcfg@scratch.dir,src.type,src.name,metadat.fname)) %>%
  #Create output file name (one file per atom)
  mutate(res.fname=sprintf("%s_%s_%s_%s.rds",src.type,src.name,sp.name,stat.name)) %>%
  #Simplify
  select(src.id,sp.id,stat.id,src.type,src.name,stat.name,sp.name,metadat.path,res.fname)

#Now use nesting trick to keep the total number of jobs at a suitable level.
#LSB can accept up 1024 jobs, but the specification should be maximum of 255 characters - 
#this corresponds to a string 1,2,3,4,5...,88 i.e. Maximum of 88 jobs. We could do
#something about this, but 88 is anyway not so bad.
max.jobs <- 88
#Nest in all different permutations, then choose the largest version under max.jobs
#TODO: An alternative approach here could be based on the processing size. But I'm
#not sure that it would help
nests.l <- list(nest(todo.tbl,data=-src.id),
                nest(todo.tbl,data=-sp.id),
                nest(todo.tbl,data=-stat.id),
                nest(todo.tbl,data=c(-src.id,-sp.id)),
                nest(todo.tbl,data=c(-src.id,-stat.id)),
                nest(todo.tbl,data=c(-stat.id,-sp.id)),
                nest(todo.tbl,data=c(-src.id,-sp.id,-stat.id)))
nest.sizes <- 
  purrr::map_dbl(nests.l,nrow) %>%
  ifelse(.>max.jobs,NA,.)

if(all(is.na(nest.sizes))) {
  stop("No solution to nesting problem.")
} else {
  do.this <- nests.l[[which.max(nest.sizes)]]
  
}

do.this$cfg.id <- seq(nrow(do.this))

#'========================================================================
# Output and setup ####
#'========================================================================
#Write configuration data
do.this %>%
  select(cfg.id) %>%
  write_csv(path = file.path(PE.cfg$dirs$job.cfg,"Stats.cfg"))
stat.cfg.dir <-file.path(PE.cfg$dirs$job.cfg,"Stats") 
if(!dir.exists(stat.cfg.dir)) dir.create(stat.cfg.dir)

#Clear the existing statistics, to avoid incomplete duplication etc
#Directory setup
stat.dir <- define_dir(pcfg@scratch.dir,PE.cfg$dirs$statistics)
unlink(dir(stat.dir,full.names=TRUE))
saveRDS(do.this,file=file.path(pcfg@scratch.dir,PE.cfg$files$stats.configuration))
#Remove todos as well
unlink(dir(file.path(PE.cfg$dirs$job.cfg,"Stats"),full.names=TRUE))

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
