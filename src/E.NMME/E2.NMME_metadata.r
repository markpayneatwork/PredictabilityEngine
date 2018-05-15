#/*##########################################################################*/
#' Collate NMME metadata 
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Wed May 18 15:58:49 2016
#'
#' Collates metadata from the NMME input files
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
cat(sprintf("\n%s\n","Collate NMME metadata"))
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
library(tibble)
library(reshape2)
library(magrittr)
load("objects/setup.RData")
load("objects/configuration.RData")

# ========================================================================
# Configuration
# ========================================================================
#Parameters
NMME.dat.dir <- file.path(pcfg@scratch.dir,"NMME","0.data")
epoch.start <- ymd("1960-01-01")

# ========================================================================
# Collate meta data
# ========================================================================
#Get list of downloaded files
NMME.fnames <- dir(NMME.dat.dir,pattern="*.nc",full.names = TRUE)

#Data storage
meta.db.l <- list()
SL.l <- list()
#Loop over files
for(f in NMME.fnames) {
  log_msg("Now retrieving metadata from %s...\n",basename(f))
  #open files
  ncid <- nc_open(f)
  #Extract meta data
  res <- tibble(filename=basename(f),
                    forecast_period=ncid$dim$L$len,
                    ensemble_members=ncid$dim$M$len,
                    first.start=min(ncid$dim$S$vals),
                    last.start=max(ncid$dim$S$vals),
                    n_starts=ncid$dim$S$len,
                    start_units=ncid$dim$S$units)
  meta.db.l[[f]] <- res
  
  #List of starts and leads
  SL.l[[basename(f)]]$S <- ncid$dim$S$vals
  SL.l[[basename(f)]]$L <- ncid$dim$L$vals
  
  #Close file
  nc_close(ncid)
}

# ========================================================================
# Process meta data
# ========================================================================
#Collate meta data
meta <- do.call(rbind,meta.db.l)
meta <- str_match(meta$filename,"^.*?_(.*)_(.*).nc")[,2:3] %>%
  set_colnames(c("model","type")) %>%
  cbind(meta) %>%
  as.tibble() %>%
  remove_rownames()

#Collate list of starts and leads
SL <- melt(SL.l) %>%
  select(filename=L1,SL=L2,value)
SL <- str_match(SL$filename,"^.*?_(.*)_(.*).nc")[,2:3] %>%
  set_colnames(c("model","type")) %>%
  cbind(SL) %>%
  as.tibble()

#Correct dates etc
meta$first.start.date <-  epoch.start  + months(meta$first.start)
meta$last.start.date <-  epoch.start  + months(meta$last.start)

#Plot overview
g <- ggplot(SL,aes(y=model))+  geom_raster(aes(x=value)) +
  facet_wrap(~SL,scales="free_x")
print(g)

#Lead distribution
g1 <- ggplot(subset(SL,SL=="L"),aes(x=value))+stat_count(width=0.5,col="black") +
      xlab("Lead time (months)")
print(g1)

#Start distribution
plt.dat <- subset(SL,SL=="S")
plt.dat$start.date <- months(plt.dat$value) + epoch.start
g2 <- ggplot(plt.dat,aes(x=start.date))+stat_count(geom="step")
print(g2)

# ========================================================================
# Complete
# ========================================================================
save(meta,file="objects/NMME_metadata.RData")

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
