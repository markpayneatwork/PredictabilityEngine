#'========================================================================
# A3.HadSLP2
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Tue Dec 29 16:25:21 2020
#
# Supports data extraction from HadSLP2 data product. Note here that we only
# use the HadSLP2 product up to 2005, following the approach of Smith et al 
# 2020. There is the HadSLP2r product that runs up to near real time, but
# this unfortunately has issues with changes in the variance strucutre. 
# Similarly, the variance-adjusted version only runs up to 2012. As we are
# not that interested in predicting NAO, but only want to quantify the 
# ratio of signals between observed and predicted, then this is good enough.
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
cat(sprintf("\n%s\n","A3.HadSLP2"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressPackageStartupMessages({
  library(PredEng)
  library(ncdf4)
})

#'========================================================================
# Configuration ####
#'========================================================================
pcfg <- readRDS(PE.cfg$path$config)

#Take input arguments, if any
set.cdo.defaults("--silent --no_warnings -O")
set.log_msg.silent()
set.nco.defaults("--overwrite")

#Data source
this.datasrc <- pcfg@Observations
assert_that(this.datasrc@name=="HadSLP2",msg="This script is only for processing HadSLP2 data")

#Working directories
analysis.grid.fname <- PE.scratch.path(pcfg,"analysis.grid")

#'========================================================================
# Setup ####
#'========================================================================
#Setup database
PE.db.delete.by.datasource(pcfg,PE.cfg$db$extract,this.datasrc)

#'========================================================================
# Import HadSLP2 data into raster ####
#'========================================================================
#Documentation is provided in the working directory
log_msg("Parsing data...\n")
dat.parse <- 
  tibble(line=readLines("data_srcs/Observations/HadSLP2/hadslp2.asc")) %>%
  mutate(line.parse=map(line,~ scan(text=.x,quiet=TRUE)),
         n.cols=map_dbl(line.parse,length),
         is.header=n.cols==2,
         raster=cumsum(is.header))

dat.r <- 
  dat.parse %>%
  #Bind into objects
  group_by(raster,is.header) %>%
  summarise(mat=list(do.call(rbind,line.parse)),
            .groups="keep") %>%  
  #Extract metadata
  mutate(is.header=ifelse(is.header,"metadata","data")) %>%
  pivot_wider(names_from="is.header",values_from=mat) %>%
  mutate(date=map(metadata,~as.Date(ISOdate(.x[1,1],.x[1,2],15)))) %>%
  unnest(date=date)%>%
  #Form rasters
  #Coordinate specifications are based on documentation
  mutate(r=map(data,~raster(.x,xmn=-182.5,xmx=177.5,ymn=-92.5,ymx=92.5)))

#Make it into a brick
HadSLP2.b <- 
  brick(dat.r$r) %>%
  setZ(z=dat.r$date,name="Date")
  

#/*======================================================================*/
#'## Extract HadSLP2 data
#/*======================================================================*/
log_msg("Extracting...\n")

#Linearly remap
dat.b <-
  raster(pcfg@global.ROI,resolution=pcfg@global.res) %>%
  resample(x=HadSLP2.b)

#'========================================================================
# Store in database ####
#'========================================================================
log_msg("Storing...\n")

#Set CRS status
#As everything is interpolated onto a common grid, it should also therefore
#have a CRS that reflects that grid
dat.b@crs <- PE.cfg$misc$crs

#Force dat.b back into memory
dat.b <- readAll(dat.b)

#Create metadata
frag.dat <- tibble(srcFname=NA,
                   srcName=this.datasrc@name,
                   srcType=this.datasrc@type,
                   realization=NA,
                   startDate=NA,
                   date=getZ(HadSLP2.b),
                   leadIdx=NA,
                   field=as.list(dat.b))

#Write to database
frag.dat %>%
  mutate(date=as.character(date)) %>%
  PE.db.appendTable(pcfg,PE.cfg$db$extract)

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
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
