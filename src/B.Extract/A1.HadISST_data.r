#/*##########################################################################*/
#' Extract HadISST data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jul 14 11:34:17 2016
#'
#' Extracts HadISST data for subsequent metric analysis. 
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

#/*======================================================================*/
#  Initialise system
#/*======================================================================*/
cat(sprintf("\n%s\n","Extract HadISST data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressPackageStartupMessages({
  library(PredEng)
})

#'========================================================================
# Configuration ####
#'========================================================================
pcfg <- PE.load.config()

#Take input arguments, if any
set.cdo.defaults("--silent --no_warnings -O")
set.log_msg.silent()
set.nco.defaults("--overwrite")

#Data source
this.datasrc <- pcfg@Observations
assert_that(this.datasrc@name=="HadISST",msg="This script is only for processing HadISST data")

#Working directories
analysis.grid.fname <- PE.scratch.path(pcfg,"analysis.grid")

#'========================================================================
# Setup ####
#'========================================================================
#Setup database
PE.db.delete.by.datasource(pcfg,PE.cfg$db$extract,this.datasrc)

#/*======================================================================*/
#'## Extract HadISST data
#/*======================================================================*/
#Extract data spatially using CDO and average
#First we need to select the grid, before doing the spatial subsetting,
#This step is not really necessary, as the analysis grid remapping will
#take care of it much more robustly
# log_msg("Subsetting data...")
# in.fname <- HadISST.dat
# temp.stem <- tempfile()
# out.fname <- temp.stem
# condexec(1,annave.cmd <- cdo(csl("sellonlatbox",as.vector(this.ROI)),
#                              "-selgrid,lonlat",
#                              in.fname,out.fname))

#Remap onto the analysis grid
log_msg("Remapping...\n")
regrid.fname <- tempfile(fileext=".nc")
remap.cmd <- cdo("-f nc", csl("remapbil", analysis.grid.fname),
                  this.datasrc@sources,regrid.fname)

#'========================================================================
# Average over MOIs (if relevant) ####
#'========================================================================
#Average over time - only necessary when considering multiple target months
#We don't need to do monthly selection, as we need the other months for
#persistence forecasts etc
if(pcfg@average.months) {
  stop("#Not run.")
  log_msg("Monthly averaging...")
  
    #monthly extraction
    tmp.out <- sprintf("%s_selmon.nc",tmp.in)
    selmon.cmd <- cdo(csl("selmon",pcfg@MOI),
                                 in.src,out.fname)
    
    #Calculate means 
    in.fname <- out.fname
    out.fname <- gsub(".nc$","_yearmean",in.fname)
    yearmean.cmd <- cdo( "yearmean", in.fname,out.fname)

}

#'========================================================================
# Import into fragments ####
#'========================================================================
log_msg("Fragmenting...\n")

#Import data into raster-land
dat.b <- readAll(brick(regrid.fname))

#Convert missing values to NAs
dat.b[dat.b < -100] <- NA

#Set CRS status
#As everything is interpolated onto a common grid, it should also therefore
#have a CRS that reflects that grid
dat.b@crs <- PE.cfg$misc$crs

#Create metadata
frag.dat <- tibble(srcFname=basename(this.datasrc@sources),
                   srcName=this.datasrc@name,
                   srcType=this.datasrc@type,
                   realization=NA,
                   startDate=NA,
                   date=getZ(dat.b),
                   leadIdx=NA,
                   field=as.list(dat.b))

#Write to database
frag.dat %>%
  mutate(date=as.character(date)) %>%
  PE.db.appendTable(pcfg,PE.cfg$db$extract)

#Remove the temporary files to tidy up
del.err <- unlink(regrid.fname)
if(del.err!=0) stop("Error deleting temp files")

# #Now, lets think for a minute. The downstream functions require two 
# #files - Anomaly_metadata.RData and Realmean_metadata.RData. The choice
# #of whether these relate to individual months or to an MOIaverage should
# #be made here, not downstream, so we therefore need to set these up according
# #to the project configuration. At the same time, we also want to store all
# #metadata, so that it can be picked up later by the persistence forcast code
# #So....
# #First generate all monthly metadata anomalies - we need this for the persistence
# #forecast anyway
# mon.anom.meta <- generate.metadata(mon.anom.dir)
# saveRDS(mon.anom.meta,file=file.path(base.dir,PE.cfg$files$Obs.monthly.anom.metadata))
# 
# #Now, setup rest of metadata accordingly
# if(pcfg@average.months) {
#   #Get metadata
#   anom.meta <- generate.metadata(MOIave.anom.dir)
# } else {
#   #We are only interested in files that are in the
#   #months of interest, so we need to filter
#   anom.meta <- subset(mon.anom.meta,month(date) %in% pcfg@MOI)
# }
# 
# #Save results and create a second copy as realmean metadata
# saveRDS(anom.meta,file=file.path(base.dir,PE.cfg$files$anom.meta))
# realmean.meta <- anom.meta  #Needs a rename
# saveRDS(realmean.meta,file=file.path(base.dir,PE.cfg$files$realmean.meta))
# 
# #And now for the climatologies
# if(pcfg@average.months) {
#   #Only a single clim file - generate by hand
#   clim.meta <- tibble(src.name=this.src@name,
#                       src.type="Climatology",
#                       date=as.Date(ISOdate(9999,pcfg@MOI,15)),
#                       start.date=NA,
# #                      n.realizations=1,
#                       fname=MOIave.clim)
#   
# } else {
#   #Generate a climatology 
#   clim.meta <- generate.metadata(mon.clim.dir)
#   clim.meta$src.type <- "Climatology"
#   
#   #Restrict to months in the MOI
#   clim.meta <- subset(clim.meta,month(date) %in% pcfg@MOI)
# }
# saveRDS(clim.meta,file=file.path(base.dir,PE.cfg$files$Obs.climatology.metadata))
# 
# #Remove the temporary files to tidy up
# tmp.fnames <- dir(work.dir,pattern=work.dir,full.names = TRUE)
# del.err <- unlink(tmp.fnames)
# if(del.err!=0) stop("Error deleting temp files")

# #/*======================================================================*/
#  Complete
#/*======================================================================*/
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
