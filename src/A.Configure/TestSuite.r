#/*##########################################################################*/
#' Test Suite Predictability Common Elements
#' ==========================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' http://www.staff.dtu.dk/mpay
#'
#' Tue Jul 12 23:05:44 2016
#'
#' Provides a very basic test suite that can be run in a few minutes on a laptop 
#' to check for the impact changes in the pipeline etc
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute"
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
# - While this script contains reminants of RMarkdown, it is not in a state
#    where it can be compiled in a meaningful manner
#/*##########################################################################*/

#'========================================================================
# Initialise system
#'========================================================================
cat(sprintf("\n%s\n","Test Suite Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
  library(tibble)
})
load(PE.cfg$path$datasrcs)

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "TestSuite",
               MOI=8,  #August
               average.months=FALSE,
               clim.years=1990:2100,   #Take in everything
               comp.years=1990:2100,
               landmask="data_srcs/NMME/landmask.nc",
               Observations=SST_obs[[c("HadISST")]],
               calibrationMethods="MeanAdj",
               NMME=NMME.sst.l)

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)
define_dir(pcfg@scratch.dir)

#'========================================================================
# Data sources ####
#'========================================================================
#Select a very limited set of decadal models
# - Two realisations per model
# - Start dates 1991-1995
# - Only two models - chosen here to be fast!
lim.dec <- 
  #Setup list of files that we have
  tibble(data.srcs=unlist(SST.Decadal.production),
         srcName=map_chr(data.srcs,slot,"name"),
         fnames=map(data.srcs,slot,"sources")) %>%
  unnest(fnames) %>%
  mutate(realization=map2_chr(data.srcs,fnames, ~ .x@realization.fn(.y)),
         startDate=map2(data.srcs,fnames,~.x@start.date(.y))) %>%
  unnest(startDate) %>%
  #Restrict
  filter(year(startDate) %in% 1991:1995,
         srcName %in% c("CESM.DPLE","NorCPM")) %>%
  arrange(srcName,startDate,realization) %>%
  group_by(srcName,startDate) %>%
  slice_head(n=2) 

#Get an overview of what we have
lim.dec %>%
  ungroup() %>%
  count(srcName,realization) %>%
  print(n=Inf)

#Add data sources into object
pcfg@Decadal <- 
  lim.dec %>%
  ungroup() %>%
  nest(data=c(-srcName)) %>%
  mutate(data.src=map2(srcName,data,
                       function(this.srcName,this.dat) {
                         this.datsrc <- SST.Decadal.production[[this.srcName]]
                         this.datsrc@sources <- this.dat$fnames
                         return(this.datsrc)
                       } )) %>%
  pull(data.src) %>%
  PElst()

  
#Select CMIP5 models
#pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="tos")
pcfg@obs.only <- FALSE

#'========================================================================
# Spatial Configurations ####
#'========================================================================
#Set global variables
pcfg@global.ROI <- extent(-70,30,50,80)
pcfg@global.res  <- 5

#Polygons
sp.objs <- list()
sp.objs$"SouthOfIceland" <- sfpolygon.from.extent(extent(-50,-10,54,70))

#Add to object
pcfg@spatial.polygons <- 
  sp.objs %>% enframe(value="geometry") %>% st_sf()


#'========================================================================
# Extraction configuration ####
#'========================================================================
#A simple point-wise extraction point (corresponding to the point of capture)
pt <- data.frame(lat=65 +42/60,
                 lon=-(30+50/60),
                 date=as.Date(c("2012-08-22","2014-08-15")))
pt$ID <- seq(nrow(pt))
pcfg@pt.extraction <-
  tibble(table=PE.cfg$db$extract,
         filter='srcType=="Observations" & srcName=="HadISST"',
         points=list(st_as_sf(pt,coords=c("lon","lat"))))

#'========================================================================
# Statistics ####
#'========================================================================
#Configure stats
stat.l <- PElst()
stat.l$threshold <- 
  threshold(name="threshold",
                         desc="11 degree threshold",
                         threshold=11,
                         above=TRUE,
                         realizations=1:4)

#Merge it all in
pcfg@statistics <- stat.l

#'========================================================================
# Finish
#'========================================================================
pcfg <- set.configuration(pcfg)

#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nConfiguration complete.\n")

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
