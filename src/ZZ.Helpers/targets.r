#'========================================================================
# makefile using targets
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Aug 20 17:03:55 2020
#
# Trigger make process using the  targets package
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
cat(sprintf("\n%s\n","makefile"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];
suppressPackageStartupMessages({
  library(targets)
})

#Take input arguments, if any
if(interactive()) {
  make.this <- character(0L) #I.e. run everything
} else {  #Running as a "function"
  make.this <- commandArgs(TRUE)

}

if(length(make.this)==0)  {
  tar_make()
} else {
  tar_make(names=!!make.this)
}

cat(sprintf("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date()))

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
