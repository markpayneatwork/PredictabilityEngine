###########################################################################
# Collect fragements metadata
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Tue May 15 21:48:28 2018
# 
# Collates metadata from the local database - the script is more or
# less the same as the version oriented to the online version, but focuses
# on the local fragments instead
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
cat(sprintf("\n%s\n","E4.Collate_local_files_metadata"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ncdf4)
library(tibble)
library(stringr)
library(dplyr)
library(ggplot2)
load("objects/setup.RData")
load("objects/configuration.RData")

#==========================================================================
# Configure
#==========================================================================
NMME.dat.dir <- file.path(pcfg@scratch.dir,"NMME")
fragment.dir <- define_dir(NMME.dat.dir,"1.fragments")

epoch.start <- ymd("1960-01-01")

set.debug.level(0)  #0 complete fresh run

#==========================================================================
# Collate data
#==========================================================================
#Get list of files
frag.fnames <- tibble(fname=dir(fragment.dir,
                              pattern = ".nc$",
                              full.names = TRUE))
frag.fnames$model <- str_match(basename(frag.fnames$fname),"^.*?_(.*?)_.*.nc$")[,2]

#Data storage
meta.db.l <- list()
pb <- progress_estimated(nrow(frag.fnames))
log_msg("Collating fragment metadata...\n")
#Loop over files
for(i in seq(nrow(frag.fnames))) {
  pb$tick()$print()
  #open file via ncdf4
  ncid <- nc_open(frag.fnames$fname[i])
  #Get contents
  d <- ncvar_get(ncid)
  #Extract meta data
  res <- tibble(start=ncid$dim$S$vals,
                lead=ncid$dim$L$vals,
                realization=underscore_field(frag.fnames$fname[i],6),
                percent.na=mean(is.na(d))) 
  meta.db.l[[i]] <- res

  #Close file
  nc_close(ncid)
}
pb$stop()$print()

# ========================================================================
# Process meta data
# ========================================================================
#Collate meta data
frag.meta <- bind_rows(meta.db.l) %>%
  add_column(model=frag.fnames$model,.before = 1) %>%
  mutate(start.date=epoch.start  + months(start),
         forecast.date=start.date+months(floor(lead)),
         forecast.month=month(forecast.date),
         forecast.year=year(forecast.date)) %>%
  add_column(fname=frag.fnames$fname) 

save(frag.meta,file=file.path(NMME.dat.dir,"Fragment_metadata.RData"))

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

