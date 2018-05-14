#/*##########################################################################*/
#' Visualise NMME climatologies
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Tue May 24 11:10:42 2016
#'
#' Viusalises the climatologies and therefore anomalies of the NMME model
#' outputs. Climatologies are calculated for each individual lead-time to
#' account for model drift.
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute" 
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
# - While this script contains reminants of RMarkdown, it is not in a state
#   where it can be compiled in a meaningful manner
#/*##########################################################################*/

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","NMME Climatologies and Anomalies"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
load("objects/common_elements.RData")
library(raster)
library(rasterVis)
library(stringr)
library(ggplot2)

# ========================================================================
# Configuration
# ========================================================================
clim.dir <- "data/NMME/5.clim/"

pdf("plots/NMME_model_shock.pdf")

# ========================================================================
# Viusalise climatologies
# ========================================================================
#Get list of files
clim.fnames <- dir(clim.dir,pattern="clim.nc$",full.names=TRUE)
f.prefix <- str_match(basename(clim.fnames),"^.*?_(.*?)_clim.nc$")[,2]

#Loop over files
for(i in seq(clim.fnames)) {
  log_msg("Processing %s...\n",f.prefix[i])
  
  #Setup raster brick
  clim.L <- brick(clim.fnames[i])
  lead.anom <- clim.L-mean(clim.L)
  lead.anom <- setZ(lead.anom,getZ(clim.L))
  
  #Do plots
  plt.dat <- gplot(clim.L)$data
  plt.dat$lead <- sprintf("Lead %04.1f months",as.numeric(gsub("X","",plt.dat$variable))+0.5)
  g <- ggplot(plt.dat)+geom_raster(aes(x=x,y=y,fill=value))+facet_wrap(~lead)+
        ggtitle(sprintf("%s climatology",f.prefix[i]))
  print(g)

  #Do plots
  names(lead.anom) <- sprintf("X%04.1f",getZ(clim.L)+0.5)
  plt.dat <- gplot(lead.anom)$data
  plt.dat$lead <- sprintf("Lead %s months",gsub("X","",plt.dat$variable))
  zlims <- range(pretty(c(plt.dat$value,-plt.dat$value)))
  g <- ggplot(plt.dat)+geom_raster(aes(x=x,y=y,fill=value))+facet_wrap(~lead)+
    ggtitle(sprintf("%s model drift",f.prefix[i])) +
    scale_fill_distiller(type="div",palette="RdBu",limits=zlims)
  print(g)
  
}

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
