###########################################################################
# Visualise fragements metadata
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Tue May 15 21:48:28 2018
# 
# Visualises the results of the local fragment database
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute" 
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
###########################################################################

#==========================================================================
# Initialise system
#==========================================================================
cat(sprintf("\n%s\n","Visualise fragment metadata"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ncdf4)
library(tibble)
library(stringr)
library(dplyr)
library(ggplot2)
load("objects/setup.RData")
load("objects/configuration.RData")

#==========================================================================
# Configure
#==========================================================================
NMME.dat.dir <- file.path(pcfg@scratch.dir,"NMME")
fragment.dir <- define_dir(NMME.dat.dir,"1.fragments")

epoch.start <- ymd("1960-01-01")

set.debug.level(0)  #0 complete fresh run

# ========================================================================
# Overview of metadata
# ========================================================================
#Setup
load(file.path(NMME.dat.dir,"NMME_fragment_metadata.RData"))

#Start distribution
plt.dat <- unique(frag.meta[,c("model","start","lead","start.date","forecast.year")])
g2 <- ggplot(plt.dat,aes(x=forecast.year,y=lead))+geom_raster()+
      facet_wrap(~model)
print(g2)

#Start-lead distributions
g3 <- ggplot(plt.dat,aes(x=start.date,y=model))+geom_raster()+
  facet_wrap(~lead)
print(g3)

#Identify suitable climatological period
g4 <- ggplot(plt.dat,aes(x=forecast.year,fill=forecast.year %in% 1983:2010))+stat_count()+
      labs(fill="Clim.year")+
      facet_wrap(~lead)
plot(g4)

#Look at proportion NA
g5 <- ggplot(frag.meta,aes(start.date,y=percent.na,group=realization))+
      geom_line()+
      facet_wrap(~model)+
  theme_bw()
print(g5)

mdl.dat <- subset(frag.meta,grepl("NASA",frag.meta$model))
g6 <- ggplot(frag.meta,aes(start.date,y=realization,colour=percent.na))+
      geom_point()+
      facet_wrap(~model,scales="free_y")+
      theme_bw()
print(g6)


#==========================================================================
# Complete
#==========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# -----------
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
# -----------

