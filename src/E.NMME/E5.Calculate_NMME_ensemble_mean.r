###########################################################################
# E5.Calculate_NMME_ensemble_mean
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed May 16 13:04:39 2018
# 
# Calculates the (unweighted) ensemble mean across the entire NMME
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute" 
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
###########################################################################

#==========================================================================
# Initialise system
#==========================================================================
cat(sprintf("\n%s\n","E5.Calculate_NMME_ensemble_mean"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
load("objects/configuration.RData")
load("objects/PredEng_config.RData")

#==========================================================================
# Configure
#==========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.id <- 1
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent(TRUE)
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--ovewrite")
} else {
  #Taking inputs from the system environment
  cfg.id <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(cfg.id=="") stop("Cannot find PBS_ARRAYID")
  #Do everything and tell us all about it
  set.debug.level(0)  #0 complete fresh run
  set.condexec.silent(FALSE)
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
}

#Other configurations
set.nco.defaults("--overwrite")

#Retrieve configurations
this.sp <- get.this.sp(file.path(PE.cfg$dirs$cfg,"NMME_Ensmean.cfg"),cfg.id,pcfg)

#Configure directories
subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
base.dir <- define_dir(subdomain.dir,"NMME")
ensmean.dir <- define_dir(base.dir,PE.cfg$files$ensmean.name)
anom.dir <- define_dir(ensmean.dir,"A.anoms")

log_msg("Configuration.\n")
log_msg("--------------\n")
log_msg("Spatial subdomain : %s (%s)\n",this.sp@name,this.sp@desc)

#==========================================================================
# Setup
#==========================================================================
#Start by loading the metadata associated with each of the NMME
#models
metadat.l <- list()
for(m in pcfg@NMME.models){
  if(class(m)=="data.source") {
    load(file.path(base.dir,m@name,PE.cfg$files$realmean.meta))
    metadat.l[[m@name]] <- realmean.meta
  }
}
metadat.all <- bind_rows(metadat.l)

#Split files into processing chunks, where the common factor is that 
#everything is the same apart from the realisation
ensmean.group <- split(metadat.all,metadat.all[,c("start.date","lead")],
                        drop=TRUE,sep="_")

#==========================================================================
# Process
#==========================================================================
#Setup
log_msg("Processing ensemble means...\n")
pb <- progress_estimated(length(ensmean.group))
ensmean.meta.l <- list()
for(em.gp in ensmean.group) {
  #Update counter
  pb$tick()$print()
  
  #Build commands
  ensmean.fname <- file.path(anom.dir,
                             gsub("^.*?(_.*)_realmean.nc$","NMME-ensmean\\1_anom.nc",em.gp$fname[1]))

  ensmean.cmd <- cdo("ensmean",em.gp$fname,ensmean.fname)
  condexec(1,ensmean.cmd)
  
  #Store new meta data
  res <- em.gp[1,] %>%
    select(-fname,-n.realizations) %>%
    mutate(n.mdls=nrow(em.gp),
           fname=ensmean.fname)
  ensmean.meta.l[[ensmean.fname]] <- res[1,]
  
}

#Form meta data
ensmean.meta <- bind_rows(ensmean.meta.l) %>%
                mutate(name="NMME-ensmean")
save(ensmean.meta,file=file.path(ensmean.dir,PE.cfg$files$realmean.meta))


#==========================================================================
# Complete
#==========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# -----------
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
# -----------

