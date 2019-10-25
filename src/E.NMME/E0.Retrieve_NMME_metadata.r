#/*##########################################################################*/
#' E0. Retrieve NMME metadata
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Mon May 23 10:45:27 2016
#'
#' Retrieves the NMME metadata directly from the servers using OpenDAP
#' Visualises the data coverage
#' 
#'  
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute" 
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
#   - Note that this script is intended to run independently from the rest of
#     the PredEng workflow, and therefore does not require (or use) a 
#     configuration object
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","E1. Retrieve NMME data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tidyverse)
library(ncdf4)

# ========================================================================
# Configuration
# ========================================================================
#Configure directories
base.dir <- define_dir(PE.cfg$dirs$datasrc,"NMME")

#'========================================================================
# Setup ####
#'========================================================================
#Import configurations from config object
NMME.cfg <- read_csv2(file.path(PE.cfg$dirs$datasrc,"NMME","NMME_SST_urls.csv"),
                      col_types = cols())

# ========================================================================
# Retrieve meta data
# ========================================================================
#Setup storage
meta.l <- vector("list",nrow(NMME.cfg))
SLM.l <- vector("list",nrow(NMME.cfg))

for(cfg.no in seq(nrow(NMME.cfg))) {
  #Choose source to work with<
  this.src <-NMME.cfg[cfg.no,] %>% mutate(mdl.str=sprintf("%s_%s",Model,type))
  
  #open file via OpenDAP
  log_msg("Now retrieving metadata from %s...\n",this.src$mdl.str)
  ncid <- nc_open(this.src$URL)
  #Extract meta data
  meta.l[[cfg.no]] <-
    tibble(forecast_period=ncid$dim$L$len,
                 ensemble_members=ncid$dim$M$len,
                 first.start=min(ncid$dim$S$vals),
                 last.start=max(ncid$dim$S$vals),
                 n.starts=ncid$dim$S$len,
                 n.leads=ncid$dim$L$len,
                 start_units=ncid$dim$S$units) %>%
    bind_cols(this.src)
  
  
  #List of starts and leads
  SLM.l[[cfg.no]] <- list(S=ncid$dim$S$vals,
              L=ncid$dim$L$vals,
              M=ncid$dim$M$vals)
  
  #Close file
  nc_close(ncid)
}

#Merge
meta <- 
  bind_rows(meta.l) %>%
  mutate(start.gaps=(last.start-first.start+1)-n.starts)

#Visualise
ggplot(meta,aes(x=first.start,xend=last.start,y=Model,yend=Model,col=type))+
  geom_segment()+
  facet_wrap(~active)+
  geom_vline(xintercept=max(meta$last.start))+
  geom_vline(xintercept=min(meta$first.start))+
  theme_bw()

saveRDS(meta,file="objects/NMME_metadata.rds")

# ========================================================================
# Complete
# ========================================================================
#+ results='asis'
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
