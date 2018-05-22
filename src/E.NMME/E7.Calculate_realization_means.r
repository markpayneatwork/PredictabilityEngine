###########################################################################
# E7.Calculate_realization_means
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed May 16 12:31:00 2018
# 
# Calculates the realization means from the anomaly fragments
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
cat(sprintf("\n%s\n","E7.Calculate_realization_means"))
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
anom.dir <- define_dir(base.dir,"A.anoms")
realmean.dir <- define_dir(base.dir,"B.realmean")

load(file.path(base.dir,"Anom_metadata.RData"))

set.debug.level(0) #Do all

#==========================================================================
# Setup
#==========================================================================
#Split files into processing chunks, where the common factor is that 
#everything is the same apart from the realisation
realmean.group <- split(anom.meta,anom.meta[,c("model","start","lead")],
                        drop=TRUE,sep="_")

#==========================================================================
# Process
#==========================================================================
#Setup
log_msg("Processing realization means...\n")
pb <- progress_estimated(length(realmean.group))
realmean.meta.l <- list()
for(rl.gp in realmean.group) {
  #Update counter
  pb$tick()$print()
  
  #Build commands
  realmean.fname <- unique(gsub("_r.*?_anom.nc","_rmean_anom.nc",rl.gp$anom.fname))
  realmean.cmd <- nces("--overwrite --netcdf4 --history",
                       file.path(anom.dir,rl.gp$anom.fname),
                       file.path(realmean.dir,realmean.fname))
  condexec(1,realmean.cmd)
  
  #Store new meta data
  res <- rl.gp %>%
    select(-realization,-frag.fname,-clim.fname,-anom.fname) %>%
    mutate(n.reals=nrow(rl.gp),
           realmean.fname=realmean.fname)
  realmean.meta.l[[realmean.fname]] <- res[1,]

}

#Form meta data
realmean.meta <- bind_rows(realmean.meta.l)
save(realmean.meta,file=file.path(base.dir,"Realmean_metadata.RData"))

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

