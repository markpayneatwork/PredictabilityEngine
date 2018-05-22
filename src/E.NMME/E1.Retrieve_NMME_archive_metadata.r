#/*##########################################################################*/
#' Retrieve NMME archive metadata 
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Wed May 18 15:58:49 2016
#'
#' Retrieves metadata from the NMME archive
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute" 
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
# - While this script contains reminants of RMarkdown, it is not in a state
#    where it can be compiled in a meaningful manner
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Retrieve NMME metadata"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(ClimateTools)
library(ncdf4)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
library(tibble)
library(reshape2)
library(magrittr)
library(reshape2)
load("objects/setup.RData")
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
NMME.dir <- define_dir(pcfg@scratch.dir,"NMME")

epoch.start <- ymd("1960-01-01")

set.debug.level(0)  #0 complete fresh run

# ========================================================================
# Retrieve meta data
# ========================================================================
#Import configurations from config object
NMME.cfg <- lapply(pcfg@NMME.models,function(d) {
                      tibble(Model=d@name,
                             type=names(d@source),
                             URL=d@source)}) %>%
            bind_rows %>%
            mutate(mdl.str=sprintf("%s_%s",Model,type))

#Data storage
meta.db.l <- list()
SLM.l <- list()
#Loop over files
for(i in seq(nrow(NMME.cfg))) {
  mdl <- NMME.cfg[i,]
  log_msg("Now retrieving metadata from %s...\n",mdl$mdl.str)
  #open file via OpenDAP
  ncid <- nc_open(mdl$URL)
  #Extract meta data
  res <- tibble(forecast_period=ncid$dim$L$len,
                    ensemble_members=ncid$dim$M$len,
                    first.start=min(ncid$dim$S$vals),
                    last.start=max(ncid$dim$S$vals),
                    n.starts=ncid$dim$S$len,
                    n.leads=ncid$dim$L$len,
                    start_units=ncid$dim$S$units) %>%
        bind_cols(mdl)
  meta.db.l[[mdl$mdl.str]] <- res
  
  #List of starts and leads
  SLM.l[[mdl$mdl.str]]$S <- ncid$dim$S$vals
  SLM.l[[mdl$mdl.str]]$L <- ncid$dim$L$vals
  SLM.l[[mdl$mdl.str]]$M <- ncid$dim$M$vals
  
  #Close file
  nc_close(ncid)
}

# ========================================================================
# Process meta data
# ========================================================================
#Collate meta data
meta <- bind_rows(meta.db.l)

#Collate list of starts and leads
SLM <- do.call(cbind,SLM.l)

#Correct dates etc
meta$first.start.date <-  epoch.start  + months(meta$first.start)
meta$last.start.date <-  epoch.start  + months(meta$last.start)

# #Plot overview
# g <- ggplot(SL,aes(y=model))+  geom_raster(aes(x=value)) +
#   facet_wrap(~SL,scales="free_x")
# print(g)
# 
# #Lead distribution
# g1 <- ggplot(subset(SL,SL=="L"),aes(x=value))+stat_count(width=0.5,col="black") +
#       xlab("Lead time (months)")
# print(g1)
# 
# #Start distribution
# plt.dat <- subset(SL,SL=="S")
# plt.dat$start.date <- months(plt.dat$value) + epoch.start
# g2 <- ggplot(plt.dat,aes(x=start.date))+stat_count(geom="step")
# print(g2)

# ========================================================================
# Complete
# ========================================================================
save(meta,SLM,epoch.start,file=file.path(NMME.dir,"NMME_archive_metadata.RData"))

#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

#' -----------
#' <small>*This work by Mark R Payne is licensed under a  Creative Commons
#' Attribution-NonCommercial-ShareAlike 3.0 Unported License. 
#' For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
#' Basically, this means that you are free to "share" and "remix" for 
#' non-commerical purposes as you see fit, so long as you "attribute" me for my
#' contribution. Derivatives can be distributed under the same or 
#' similar license.*</small>
#'
#' <small>*This work comes with ABSOLUTELY NO WARRANTY or support.*</small>
#'
#' <small>*This work should also be considered as BEER-WARE. For details, see
#' http://en.wikipedia.org/wiki/Beerware*</small>
#' 
#' -----------
#
# Fin
