#'========================================================================
# F2.Retrieve_metadata.r
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed May 23 10:50:49 2018
#
# Extracts metadata from the fragments created in script F1, for use in subsequent
# processing
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
cat(sprintf("\n%s\n","F2.Retrieve_metadata.r"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(PredEng)
library(dplyr)
library(lubridate)
library(ncdf4)
library(tibble)
library(tidyr)
load("objects/configuration.RData")

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  set.debug.level(Inf)  #0 complete fresh run
  set.condexec.silent()
  set.cdo.defaults("--silent -O --no_warnings")
  set.log_msg.silent(TRUE)
} else {
  #Taking inputs from the system environment
  #  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  #if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.debug.level(0)  #0 complete fresh run
  set.log_msg.silent(FALSE)
}
set.nco.defaults("--netcdf4 --overwrite --history")


#Directory setup
src.dir <- file.path("data_srcs","CMIP5")
base.dir <- define_dir(pcfg@scratch.dir,"CMIP5")
frag.dir <- define_dir(base.dir,"3.fragments")
fragstack.dir <- define_dir(base.dir,"4.fragstacks")
clim.dir <- define_dir(base.dir,"5.clim")
anom.dir <- define_dir(base.dir,"A.anom")
realmean.dir <- define_dir(base.dir,"B.realmean")
ensmean.dir <- define_dir(base.dir,"C.ensmean")

#Check that climatology range is valid
if(any(pcfg@clim.years>2005))stop("Climatology years extend beyond 2005.")

#'========================================================================
# Setup ####
#'========================================================================
#Get list of fragments
frag.fnames <- dir(frag.dir,pattern=".nc",full.names = TRUE,recursive=TRUE)
if(length(frag.fnames)==0) stop("Cannot find fragment files")

#Fragment metadata

#'========================================================================
# Retrieve metadata ####
#'========================================================================
#Loop over the individual files getting the meta information
log_msg("Preparing metadata\n")


#We used to do the date extraction directly out of the files
#but now that we are doing the fragmenting down to 2D fields, that
#is no longer necessary - we can just take it directly from the filename
#At some point we should do a check that they line up, but its not 
#directly necessary at the moment
# date.meta.l  <- list()
# pb <- progress_estimated(length(frag.fnames))
# for(f in frag.fnames) {
#   pb$tick()$print()
#   log_msg("Processing metadata for %s...\n",basename(f),silenceable = TRUE)
# 
#   #Doing it with a NetCDF read is faster, but doesn't
#   #do a very good job of parsing the dates correctly
#   #We use raster instead
#   b <- brick(f)
#   b.dates <- getZ(b)
#   date.meta.l[[f]] <-  tibble(date= b.dates)  
# }
# Sys.sleep(0.1)
# print(pb$stop())
# log_msg("\n")

#Form metadata table
frag.meta <- tibble(name=underscore_field(frag.fnames,1),
                    expt=underscore_field(frag.fnames,2),
                    realization=underscore_field(frag.fnames,3),
                    date=as.Date(ISOdate(underscore_field(frag.fnames,4),
                                         pcfg@MOI,15))) %>%
  add_column(fname=frag.fnames)

#Extract out the individual realization numbers as well. At some point it
#might be good to store these in the fragstacks
frag.meta <- extract(frag.meta,"realization",
                     c("realization.r","realization.i","realization.p"),
                     "r([0-9]+)i([0-9]+)p([0-9]+)",
                     remove=FALSE) %>%
             mutate(realization.r =as.numeric(realization.r))

#Sort, for good measure
frag.meta <- arrange(frag.meta,name,expt,date,realization.r)

save(frag.meta,file=file.path(base.dir,"Fragment_metadata.RData"))


#'========================================================================
# Generate fragment stacks ####
# Now we stack the fragments together to form 3D stacks, with each lon-lat
# layer corresponding to a realisation
#'========================================================================
log_msg("Building fragstacks...\n")
# Group data into the fragment stacks
fragstack.grp <- split(frag.meta,
                       frag.meta[,c("date","expt","name")],drop=TRUE)

#Loop over groups and build the stacks
pb <- progress_estimated(length(fragstack.grp))
fragstack.meta.l <- list()
for(i in seq(fragstack.grp)) {
  pb$tick()$print()
  grp <- fragstack.grp[[i]]
  log_msg("Building fragstack %i of %i...\n",i,
          length(fragstack.grp),silenceable = TRUE)
  
  #Stack
  fragstack.fname <- file.path(fragstack.dir,
                               with(grp[1,],
                                    sprintf("%s_%s_%s_fragstack.nc",
                                            name,expt,year(date))))
  # condexec(1,fragstack.cmd <- cdo("merge",
  #                                 grp$fname,
  #                                 fragstack.fname))
  condexec(1,fragstack.cmd <- ncecat("--rcd_nm realization -M",
                                     grp$fname,fragstack.fname))

  #Store metadata
  fragstack.meta.l[[i]] <- grp[1,] %>%
                          mutate(n.realizations=nrow(grp),
                                 fname=fragstack.fname)

}
Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

#Save metadata
fragstack.meta <- bind_rows(fragstack.meta.l) %>%
                  select(-starts_with("realization")) 
save(fragstack.meta,file=file.path(base.dir,"Fragstack_metadata.RData"))

#'========================================================================
# Calculate climatologies ####
# The climatology is a bit messy to calculate in this context, because of
# the fact that we have the historical experiments that end in 2005 and 
# then we split into multiple rcp experiments. We simplify this problem
# by focusing only on the historical experiment for the purpose of defining
# the climatology - we also make this redefinition through the rest of
# the configuration. This may cause some problems in the future, although
# it is hard to see at the moment where, because we have a comparison
# period as well, which is used for estimating skill metrics. Anyway,
# we simplify things by not allowing climatologies to extend beyond 2005
# and then proceed accordingly
#'========================================================================
log_msg("Calculating climatologies...\n")

#Setup metadata table for next round of processing (climatologies and anoms)
anom.meta <-  mutate(fragstack.meta,
                     type=sprintf("%s.%s","CMIP5",expt),
                     fragstack.fname=fname,
                     fname=file.path(anom.dir,sprintf("%s_%s_%s_anom.nc",name,expt,year(date))),
                     clim.fname=file.path(clim.dir,sprintf("%s_clim.nc",name)))

#Now select the files to work with. As we have fragstacks, we can select for
#both year and experiment simultaneously
hist.meta <- subset(anom.meta,expt=="historical" & year(date) %in% pcfg@clim.years)
clim.grp.l <- split(hist.meta,hist.meta[,c("name")])

# Calculate climatologies
# Calculating the climatology could be complicated by the fact
# that not all of the fragstacks that would go into the climatology 
# necessarily have the same number of realizations. If not, then this
# requires averaging over the fragstack first and then over the years.
# We check for this along the way
pb <- progress_estimated(length(clim.grp.l))
for(i in seq(clim.grp.l)) {
  pb$tick()$print()
  log_msg("Calculating climatology %i of %i...\n",i,
          length(clim.grp.l),silenceable = TRUE)
  g <- clim.grp.l[[i]]
  
  #Check that all fragstacks have the same number of realizations. If not, 
  #throw an error (to start with)
  unique.n.reals <- table(g$n.realizations)
  if(length(unique.n.reals)!=1) {
    stop("Uneven number of realizations in selected fragstacks")
  }
  
  #Calculate the climatology
  #We need to use nco here, rather than CDO - not sure why, but it
  #seems that the dimensionality is a bit too strange for CDO
  #First we do the ensemble averaging, then we need to average
  #over the realization climatologies to get the total climatology
  #Note that we need to do the averaging with ncwa rather than ncra so 
  #anomaly creation via ncdiff works properly
  realclim.tmp <- tempfile()
  condexec(5,realclim.cmd <- nces(g$fragstack.fname,realclim.tmp))
  condexec(5,clim.cmd <- ncwa(realclim.tmp,unique(g$clim.fname)))
  
}

Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

#'========================================================================
# Calculate anomalies ####
#'========================================================================
log_msg("Anomalies...\n")

#Calculate the anomalies per realisation
pb <- progress_estimated(nrow(anom.meta))
for(m in seq(nrow(anom.meta))){
  pb$tick()$print()
  am <- anom.meta[m,]
  log_msg("Calculating anomalies for %s...\n",
          basename(am$fname),silenceable = TRUE)

  #Calculate the anomaly
  #condexec(6,anom.cmd <- cdo("sub",am$fragstack.fname,am$clim.fname,am$fname))
  condexec(6,anom.cmd <- ncdiff(am$fragstack.fname,am$clim.fname,am$fname))
}
Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

save(anom.meta,file=file.path(base.dir,"Anom_metadata.RData"))

#'========================================================================
# Calculate realisation means ####
# This should be simple, because we have everything in place in the form
# of fragstack anomalies - we just average over them
#'========================================================================
log_msg("Realisation means...\n")

#Break into chunks per model, experiment
realmean.meta <- anom.meta %>%
                 mutate(anom.fname=fname,
                        fname=file.path(realmean.dir,
                                        gsub("anom.nc",
                                             "realmean-anom.nc",
                                             basename(fname))),
                        n.realizations=1,
                        fragstack.fname=NULL)

#Average over the fragstack anomalies at given lead time
pb <- progress_estimated(nrow(realmean.meta))
for(i in seq(nrow(realmean.meta))) {
  pb$tick()$print()
  rlm <- realmean.meta[i,]
  log_msg("Calculating realmean %i of %i...\n",
          i,nrow(realmean.meta),
          silenceable = TRUE)

  #Perform averaging over realizations
  condexec(7,realmean.cmd <- ncwa("-a realization",
                                  rlm$anom.fname,rlm$fname))
  
}

Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

#Save realmeta data
save(realmean.meta,file=file.path(base.dir,"Realmean_metadata.RData"))


#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("Analysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

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
