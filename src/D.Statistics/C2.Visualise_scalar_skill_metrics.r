#'========================================================================
# C2.Visualise stats and metrics.r
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
cat(sprintf("\n%s\n","Visualise stats and metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
start.time <- proc.time()[3]; 

#Helper functions, externals and libraries
library(tidyverse)
library(rmarkdown)
library(PredEng)
pcfg <- readRDS(PE.cfg$path$config)

#'========================================================================
# Configure ####
#'========================================================================
Rmd.src <- here("src/D.Statistics/C2.Visualise_scalar_skill_metrics.Rmd")

#'========================================================================
# Setup ####
#'========================================================================
date.time.str <- format(Sys.time(),"%Y%m%d_%H%M%S")
out.fname <- file.path(sprintf("%s_stats_and_skill_metrics_%s.html",pcfg@project.name,date.time.str))
out.dir <- define_dir(pcfg@scratch.dir,"notebooks")
html.fname <- render(Rmd.src,
                     knit_root_dir = getwd(),
                     intermediates_dir=out.dir,
                     output_dir=out.dir,
                     output_file=out.fname,
                     params=list(set_title=sprintf("%s Statistics and Skill",
                                                   gsub("_"," ",pcfg@project.name))),
                     clean=FALSE,
                     encoding="UTF-8",
                     envir=new.env())

#Copy results to notebooks directory
file.copy(file.path(out.dir,out.fname),"notebooks",overwrite=TRUE)


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
