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
})
these.srcs <- readRDS(PE.cfg$path$datasrcs)

#'========================================================================
# Project Configuration ####
#'========================================================================
#Global project configuration
pcfg <- PredEng.config(project.name= "TestSuite",
                       MOI=8,  #August
                       average.months=FALSE,
                       clim.years=1991:2000,   #Take in everything
                       comp.years=1990:2010,
                       landmask="data_srcs/NMME/landmask.nc",
                       calibrationMethods=c("MeanAdj"),
                       persistence.leads = seq(7,120,by=12))

#Setup scratch directory
pcfg@scratch.dir <- file.path(PE.cfg$dir$scratch,pcfg@project.name)
define_dir(pcfg@scratch.dir)

#'========================================================================
# Data sources ####
#'========================================================================
#' Observations
pcfg@Observations <- 
  filter(these.srcs,
         group=="SST.obs",srcName=="HadISST") %>%
  pull(sources) %>%
  pluck(1)
  
#'
#Select a very limited set of decadal models
# - Two realisations per model
# - Start dates 1991-2005
# - Only two models - chosen here to be fast!
lim.dec <- 
  #Setup list of files that we have
  these.srcs %>%
  filter(group=="SST.Decadal" | group=="CMIP6" & var=="tos") %>%
  mutate(fnames=map(sources,slot,"sources")) %>%
  unnest(fnames) %>%
  mutate(realization=map2_chr(sources,fnames, ~ .x@realization.fn(.y)),
         startDate=map2(sources,fnames,~.x@start.date(.y))) %>%
  unnest(startDate) %>%
  #Restrict
  filter(year(startDate) %in% 1991:1992 | is.na(startDate),
         srcName %in% c("NorCPM","CESM.DPLE","historical")) %>%
  arrange(srcName,startDate,realization) %>%
  group_by(srcName,startDate) %>%
  slice_head(n=2) %>%
  ungroup() 

#Get an overview of what we have
lim.dec %>%
  count(srcName,realization) %>%
  print(n=Inf)

#Apply restricted list of files back on to the sources list
pcfg@Models <-
  lim.dec %>%
  select(-sources,-realization,-startDate) %>%
  nest(fnames=c(fnames)) %>%
  left_join(y=these.srcs) %>%
  mutate(mod.data.src=map2(sources,fnames,
                       function(this.src,this.fnames) {
                         this.src@sources <- this.fnames$fnames
                         return(this.src)
                       } )) %>%
  pull(mod.data.src) %>%
  PElst()

#Select CMIP5 models
#pcfg@CMIP5 <- make.CMIP5.srcs(CMIP5.db,var="tos")
pcfg@obs.only <- FALSE

#Set short version of HadISST data
pcfg@Observations@sources <- here("data_srcs/Observations/HadISST/HadISST_sst_short.nc")

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
            retain.field = TRUE,
            above=TRUE)

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
