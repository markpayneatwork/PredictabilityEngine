#'========================================================================
# H4.Visualise_summary_stats.r
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu May  2 13:34:17 2019
#
# <Description>
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
cat(sprintf("\n%s\n","H4.Visualise_summary_stats.r"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(tidyverse)
library(rmarkdown)
library(PredEng)

#'========================================================================
# Configure ####
#'========================================================================
pcfg <- readRDS(PE.cfg$config.path)
Rmd.src <- "src/H.Summary_statistics/H4.Visualise_summary_and_skill_stats.Rmd"

#'========================================================================
# Setup ####
#'========================================================================
out.fname <- file.path(sprintf("%s_summary_and_skill.html",pcfg@project.name))
html.fname <- render(Rmd.src,
                     knit_root_dir = getwd(),
                     output_file=out.fname,
                     output_dir="notebooks",
                     clean=TRUE,
                     params=list(title="test"),
                     encoding="UTF-8",
                     envir=new.env())

if(interactive()) rstudioapi::viewer(html.fname)

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

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
