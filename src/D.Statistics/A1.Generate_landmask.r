#'========================================================================
# A2.Generate_landmask
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Tue Apr  6 08:57:05 2021
#
# Generates the landmask to be used later by statistics, based on the
# climatological fields
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
cat(sprintf("\n%s\n","A2.Generate_landmask"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressMessages({
  library(PredEng)
})
pcfg <- PE.load.config()

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any

#'========================================================================
# Setup ####
#'========================================================================
# Setup input data
# Note that we don't subset by statistic type - we need to be able to
# calculate all statistics in principle 
clim.tbl <- 
  PE.db.tbl(pcfg,PE.cfg$db$climatology,this.datasrc)  

clim.dat <-
  clim.tbl %>%
  filter(month %in% !!pcfg@MOI) %>%
  collect() %>%
  PE.db.unserialize()

#'========================================================================
# Calculate climatologies ####
#'========================================================================
# Calculate the landmask per datasource. We could just select one, but on
# the other hand, it doesn't hurt to check
datsrc.mask <- 
  clim.dat %>%
  nest(data=-c(srcType,srcName)) %>%
  mutate(mask=map(data,~any(is.na(brick(.x$field))))) 

#Make a plot
plt.datsrc.mask <-
  datsrc.mask %>%
  mutate(plt.dat=map(mask,~rasterToPoints(.x) %>% as_tibble()))%>%
  select(-data,-mask) %>%
  unnest(plt.dat) %>%
  unite("srcTypeName",srcType,srcName,sep="/") %>%
  ggplot(aes(x,y,fill=layer))+
  geom_raster()+
  facet_wrap(~srcTypeName)+
  annotation_map(map_data("world"),colour="white",fill=NA)+
  coord_quickmap(expand=FALSE)+
  labs(fill="Mask",x="",y="")+
  ggtitle("Landmasks by datasource")+
  expand_limits(fill=c(0,1))

#Merge into a consensus mask
consensus.mask <- 
  datsrc.mask$mask %>%
  brick() %>%
  sum()

#And make another map
plt.consensus <-
  consensus.mask %>%
  rasterToPoints() %>%
  as_tibble() %>%
  ggplot(aes(x,y,fill=layer))+
  geom_raster()+
  scale_fill_viridis_c()+
  annotation_map(map_data("world"),colour="white",fill=NA)+
  coord_quickmap(expand=FALSE)+
  labs(fill="Number of\ndata sources",x="",y="")+
  stat_contour(mapping=aes(x,y,z=layer),breaks=0.5,colour="red")+
  ggtitle("Consensus landmask")+
  expand_limits(fill=c(0,1))

#'========================================================================
# Complete ####
#'========================================================================
# Save landmask to rds
(consensus.mask !=0) %>%
saveRDS(file=PE.scratch.path(pcfg,"landmask"))

# Save plots
pdf(PE.scratch.path(pcfg,"landmask.pdf"))
plot(plt.datsrc.mask)
plot(plt.consensus)
dev.off()

#Turn off the lights
dbDisconnect(clim.tbl)
if(length(warnings())!=0) print(warnings())
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
