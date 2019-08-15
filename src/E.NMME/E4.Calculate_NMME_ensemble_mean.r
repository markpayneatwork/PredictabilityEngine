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
library(pbapply)
pcfg <- readRDS(PE.cfg$config.path)

#==========================================================================
# Configure
#==========================================================================
#Take input arguments, if any
if(interactive()) {
  cfg.id <- 1
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--ovewrite")
  options("mc.cores"= 7)
} else {
  #Taking inputs from the system environment
  cfg.id <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.id=="") stop("Cannot find LSB_JOBINDEX")
  #Do everything and tell us all about it
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
  options("mc.cores"= as.numeric(Sys.getenv("LSB_MAX_NUM_PROCESSORS"))-1)
}

#Other configurations
set.nco.defaults("--overwrite")

#Retrieve configurations
this.sp <- configure.sp(file.path(PE.cfg$dirs$job.cfg,"NMME_ensmean.cfg"),cfg.id,pcfg)
config.summary(pcfg,this.sp)

#Configure directories
subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
base.dir <- define_dir(subdomain.dir,"NMME")
ensmean.dir <- define_dir(base.dir,PE.cfg$files$ensmean.name)
anom.dir <- define_dir(ensmean.dir,"A.anoms")

#==========================================================================
# Setup
#==========================================================================
#Start by loading the metadata associated with each of the NMME
#models
metadat.l <- list()
for(m in pcfg@NMME){
  if(class(m)=="data.source") {
    metadat.l[[m@name]] <- readRDS(file.path(base.dir,m@name,PE.cfg$files$realmean.meta))
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
#Process ensemble means
log_msg("Processing ensemble means...\n")
ensmean.fn <- function(em.gp){
  #Build commands
  ensmean.fname <- file.path(anom.dir,
                             gsub("^.*?(_.*)_realmean.nc$","NMME-ensmean\\1_anom.nc",basename(em.gp$fname[1])))

  ensmean.cmd <- cdo("ensmean",em.gp$fname,ensmean.fname)

  #Store new meta data
  res <- em.gp[1,] %>%
    select(-fname,-n.realizations) %>%
    mutate(n.mdls=nrow(em.gp),
           fname=ensmean.fname)
  return(res[1,])
  
}
ensmean.meta.l <- pblapply(ensmean.group,ensmean.fn,cl=getOption("mc.cores"))

#Form meta data
ensmean.meta <- bind_rows(ensmean.meta.l) %>%
                mutate(name="NMME-ensmean")
saveRDS(ensmean.meta,file=file.path(ensmean.dir,PE.cfg$files$realmean.meta))


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

