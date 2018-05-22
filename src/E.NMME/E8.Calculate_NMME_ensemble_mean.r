###########################################################################
# E8.Calculate_NMME_ensemble_mean
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
cat(sprintf("\n%s\n","E8.Calculate_NMME_ensemble_mean"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(dplyr)
load("objects/configuration.RData")
load("objects/setup.RData")

#==========================================================================
# Configure
#==========================================================================
base.dir <- file.path(pcfg@scratch.dir,"NMME")
realmean.dir <- define_dir(base.dir,"B.realmean")
ensmean.dir <- define_dir(base.dir,"C.ensmean")

load(file.path(base.dir,"NMME_realmean_metadata.RData"))

set.debug.level(0) #Do all

#==========================================================================
# Setup
#==========================================================================
#Split files into processing chunks, where the common factor is that 
#everything is the same apart from the realisation
ensmean.group <- split(realmean.meta,realmean.meta[,c("start","lead")],
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
  ensmean.fname <- unique(sprintf("NMME_ensmean_all_%s_%s_ensmean_anom.nc",
                                  underscore_field(em.gp$realmean.fname,4),
                                  underscore_field(em.gp$realmean.fname,5)))
  ensmean.cmd <- nces("--overwrite --netcdf4 --history",
                       file.path(realmean.dir,em.gp$realmean.fname),
                       file.path(ensmean.dir,ensmean.fname))
  condexec(1,ensmean.cmd)
  
  #Store new meta data
  res <- em.gp %>%
    select(-realmean.fname,-n.reals) %>%
    mutate(n.mdls=nrow(em.gp),
           ensmean.fname=ensmean.fname)
  ensmean.meta.l[[ensmean.fname]] <- res[1,]
  
}

#Form meta data
ensmean.meta <- bind_rows(ensmean.meta.l)
save(ensmean.meta,file=file.path(base.dir,"NMME_ensmean_metadata.RData"))

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

