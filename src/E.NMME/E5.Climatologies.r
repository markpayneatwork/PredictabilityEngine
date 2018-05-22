###########################################################################
# E5.Climatologies
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed May 16 08:37:49 2018
# 
# Calculates the climatologies of the NMME 
# forecast data, based on the exploded fragments
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
cat(sprintf("\n%s\n","E5.Climatologies"))
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
lead.clim.dir <- define_dir(base.dir,"4.lead.clims")

load(file.path(base.dir,"Fragment_metadata.RData"))

set.debug.level(0) #Do all

#==========================================================================
# Setup
#==========================================================================
#Modify meta data to include climatology and anomaly filenames
anom.meta <- mutate(frag.meta,
                    frag.fname=fname,fname=NULL,
                    clim.fname=sprintf("NMME_%s_L%s_clim.nc",model,lead),
                    anom.fname=gsub(".nc","_anom.nc",basename(frag.fname)))

#Define climatology file to use, based on grouping by  lead time and model
in.clim <- subset(anom.meta,forecast.year %in% pcfg@clim.years)
clim.files.l <- split(in.clim,in.clim$clim.fname)

#==========================================================================
# Calculate climatologies
#==========================================================================
#Loop over climatological files
log_msg("Generating climatologies...\n")
pb <- progress_estimated(length(clim.files.l))
for(cf in clim.files.l) {
  #Update progress bar
  pb$tick()$print()
  
  #Setup   
  clim.out.fname <- file.path(lead.clim.dir,unique(cf$clim.fname))

  #Calculate climatology using nces
  clim.cmd <- nces("--netcdf4 --overwrite --history ",
                   cf$frag.fname,
                   clim.out.fname)
  condexec(1,clim.cmd)
  
  #Apply ncwa to remove degenerate dimensions
  ncwa.cmd <- ncwa("--overwrite -a sst,S,L,M",
                   clim.out.fname,clim.out.fname)
  condexec(1,ncwa.cmd)
  
}

save(anom.meta,file=file.path(base.dir,"Anom_metadata.RData"))

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

