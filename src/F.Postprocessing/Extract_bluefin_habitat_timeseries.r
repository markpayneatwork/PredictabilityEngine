#'========================================================================
# Extract_bluefin_habitat_timeseries
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed Nov  6 15:12:26 2019
#
# Extracts the Bluefin habitat timeseries and saves it to a CSV for Brian
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
cat(sprintf("\n%s\n","Extract_bluefin_habitat_timeseries"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)

#'========================================================================
# Configure ####
#'========================================================================
#Take input arguments, if any
bluefin.dir <- "scratch/Bluefin/"

#'========================================================================
# Setup ####
#'========================================================================
stats <- readRDS(file.path(bluefin.dir,"All_scalar_stats.rds"))

sel <- 
  stats %>%
  filter(src.type=="Observations",
         sp.subdomain=="South.of.Iceland" & stat.name =="11 degree threshold - realmeans" |
         sp.subdomain=="Denmark.strait"   & stat.name =="Average temperature") %>%
  select(stat.name,date,value)

ggplot(sel,aes(x=date,y=value))+
  geom_line()+
  facet_wrap(~stat.name,scales="free_y")

#Save results
sel %>%
  spread(stat.name,value) %>%
  rename(area="11 degree threshold - realmeans",
         temperature="Average temperature") %>%
  write_csv2("outputs/Bluefin_tuna_habitat.csv")

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
