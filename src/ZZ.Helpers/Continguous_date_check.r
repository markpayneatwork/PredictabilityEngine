#'========================================================================
# Continguous_date_check
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Tue Oct 29 08:47:30 2019
#
# The timestamps on the MPI data have a bad habitat of being partially corrupted 
# (or at least nonsensical). Here we try to identify problematic cases by
# applying a test to ensure that the dates in the time variable are monotonically
# increasing.
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
cat(sprintf("\n%s\n","Continguous_date_check"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)
library(ncdf4)

#'========================================================================
# Configure ####
#'========================================================================
src.dir <- "data_srcs/Decadal/MPI-ESM-LR_MiKlip-b1/so_10member/"

#'========================================================================
# Setup ####
#'========================================================================
tb <- tibble(fname=dir(src.dir,pattern="*.nc$",full.name=TRUE,recursive = TRUE))

#Loop over files
res.l <- vector("list",length=nrow(tb))
pb <- progress_estimated(nrow(tb))
pb$print()
for(i in seq(nrow(tb))) {
  this.f <- tb$fname[i]
  ncid <- nc_open(this.f)
  time.steps <- ncid$dim$time$vals
  time.diff <- diff(time.steps)
  res.l[[i]] <- tibble(fname=this.f,
                     n.steps=ncid$dim$time$len,
                     monotonous.increasing=all(time.diff>0))
  nc_close(ncid)
  pb$tick()$print()
}

res <- bind_rows(res.l)

saveRDS(res,file="objects/Continguous_date_check.rds")

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
