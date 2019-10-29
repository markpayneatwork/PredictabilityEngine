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
# be adaptive in nature, so that it is driven by the files that are available,
# and not necessarily by how the system is configured. This is important when
# space limitations start to kick in, as not all of the result files need to be 
# present or processed for this to work.
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
these.cfgs <-  partition.workload(pcfg)

#The statistics, in principle, determine the type of data that should be used
# as an input (i.e. realmeans, realizations etc) and thereby the metadata that
# we want to collate. They  can also inform the spatial domain that we are interested in
#as well - particularly for statistics that work with  fields, instead of singular
#values, we want to process the entire global domain, rather than just a single
#local spatial domain. Hence, we need to select the spatial statistics accordingly.
stats.tb <- tibble(name=map_chr(pcfg@statistics ,slot,name="name"),
                   use.globally=map_lgl(pcfg@statistics,slot,name="use.globally"),
                   stat=pcfg@statistics,
                   returns.field=map_lgl(pcfg@statistics,returns.field),
                   use.realmeans=map_lgl(pcfg@statistics,slot,name="use.realmeans"))

#Develop a similar table for the spatial aspects
sp.tb <- tibble(name=map_chr(pcfg@spatial.subdomains,slot,name="name"),
                sp=pcfg@spatial.subdomains,
                base.dir=map_chr(sp,get.subdomain.dir,cfg=pcfg),
                is.global=name==PE.cfg$misc$global.sp.name)

#Merge everything into one large overview table
stat.cfg.all <- 
  expand.grid(cfg.id=these.cfgs$cfg.id,
            use.realmeans=unique(stats.tb$use.realmeans),
            base.dir=unique(sp.tb$base.dir)) %>%
  right_join(x=these.cfgs,by="cfg.id") %>%
  #Drop ensemble means using individual realisations (doesn't make sense) 
  filter(!(src.name==PE.cfg$files$ensmean.name & !use.realmeans)) %>%
  #Select meta data source
  mutate(metadat.fname=ifelse(use.realmeans,PE.cfg$files$realmean.meta,PE.cfg$files$anom.meta),
         metadat.path=file.path(base.dir,src.type,src.name,metadat.fname)) %>%
  select(-base.dir,-metadat.fname) %>%
  #Check that the file exists
  mutate(file.exists=file.exists(metadat.path))

#Work only with the files that exist
stat.cfg <-
  stat.cfg.all %>%
  filter(file.exists) %>%
  #Import metadata 
  mutate(metadat=map(metadat.path,readRDS),
         n.files=map_int(metadat,nrow),
         file.size=map_dbl(metadat,~ sum(file.size(.x$fname),na.rm=TRUE))) %>%
  select(-metadat)%>%
  #Only process files where we have data to process
  filter(file.size!=0) %>%
  mutate(n.chunks=ceiling(file.size/sum(file.size)*pcfg@stat.jobs)) %>%
  #Split into chunks
  mutate(chunk.idx=map(n.chunks,seq)) %>%
  unnest(chunk.idx)%>%
  mutate(cfg.id=seq(nrow(.)))

#'========================================================================
# Output and setup ####
#'========================================================================
#Write configuration data
write_csv(stat.cfg,path = file.path(PE.cfg$dirs$job.cfg,"Stats.cfg"))
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
