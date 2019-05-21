#'========================================================================
# Collate_data_src_metadata
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Tue May 21 12:34:54 2019
#
# Reads the metadata from a data source 
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
cat(sprintf("\n%s\n","Collate_data_src_metadata"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)
library(ncdf4)

#'========================================================================
# Configure ####
#'========================================================================
src.dir <- "data_srcs/Decadal/MPI-ESM-LR_MiKlip-b1/so/"

#'========================================================================
# Start processing  ####
#'========================================================================
#Setup 
fnames.l <- dir(src.dir,pattern="*.nc",full.names=TRUE)
meta.l <- list()

#Loop
pb <- progress_estimated(length(fnames.l),-1)

for(f in fnames.l) {
  pb$tick()
  ncid <- nc_open(f)
  meta.l[[f]] <- tibble(val=ncid$dim$time$vals,
                        position=seq(val))
  nc_close(ncid)
}

#Merge into a matrix
var.df <- bind_rows(meta.l,.id = "fname") %>%
          tidyr::extract(fname,"realisation","_(r[0-9]i[0-9]p[0-9])_")

ggplot(var.df,aes(x=position,y=val))+
  geom_point()+
  facet_wrap(~realisation)


#'========================================================================
# And Go ####
#'========================================================================


#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

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
