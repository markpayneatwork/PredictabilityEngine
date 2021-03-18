#/*##########################################################################*/
#' Extract EN4 data
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Fri Sep  2 14:13:44 2016
#'
#' Extracts EN4 data for subsequent metric analysis
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
cat(sprintf("\n%s\n","Extract EN4 data"))
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
assert_that(this.datasrc@name=="EN4",msg="This script is only for processing EN4 data")

#Working directories
analysis.grid.fname <- PE.scratch.path(pcfg,"analysis.grid")
tmp.dir <- tempdir()

#Choose the data_src configuration
if(!this.datasrc@name=="EN4") stop("Not configured to use EN4 data")

#/*======================================================================*/
#'## Extract EN4 meta data into fragments
#/*======================================================================*/
#Setup calibration database and clear anything that doesn't exist
PE.db.setup.extraction(pcfg,this.datasrc)
PE.db.delete.by.datasource(pcfg,PE.cfg$db$extract,this.datasrc)

log_msg("Extracting fragments...\n")

#Loop over files
pb <- PE.progress(this.datasrc@sources)

for(f in this.datasrc@sources) {
  f.tmpstem <- tempfile()
  #For each individual file, strip out as much extra info as possible
  #by selecting the field of interest, region of interest and the layers of interest
  #Select the levels and field of interest first
  if(length(pcfg@vert.range)==1 & all(is.na(pcfg@vert.range))) { 
    #Then we are requesting the surface layer only
    vert.idxs <- 1
  } else {
    #Need to work it out ourselves
    vert.idxs <- this.datasrc@z2idx(pcfg@vert.range,f)
  }
  f.sellev <- paste0(f.tmpstem,"sellev")
  sellev.cmd <- cdo(csl("sellevidx",vert.idxs),
                    csl("-selname",this.datasrc@var),
                    f,
                    f.sellev)
  
  #If we are dealing with temperature, need to convert from K to C
  if(this.datasrc@var=="temperature") {
    f.next <- paste0(f.sellev,"addC")
    levmean.cmd <- cdo("addc,-273.15",
                       f.sellev,
                       f.next)
  } else {
    f.next <- f.sellev
  }
  
  #Average over the extracted levels
  f.vertmean <- paste0(f.next,"_vertmean")
  levmean.cmd <- cdo("vertmean",
                     f.next,f.vertmean)
  
  #Do the spatial remapping and extraction
  f.remap <- paste0(f.vertmean,"_remap")
  remap.cmd <- cdo(csl("remapbil", analysis.grid.fname),
                   f.vertmean,f.remap)
  
  #Import data into raster-land
  dat.b <- readAll(brick(f.remap))
  
  #Set CRS status
  #As everything is interpolated onto a common grid, it should also therefore
  #have a CRS reflecting that grid.
  dat.b@crs <- PE.cfg$misc$crs
  
  #Create metadata
  frag.data <- tibble(srcFname=basename(f),
                      srcName=this.datasrc@name,
                      srcType=this.datasrc@type,
                      date=this.datasrc@date.fn(f.remap),
                      field=as.list(dat.b))
  
  #Write to database
  frag.data %>%
    mutate(date=as.character(date)) %>%
    PE.db.appendTable(pcfg,PE.cfg$db$extract,this.datasrc,dat=.)
  
  #Tidy up
  tmp.fnames <- dir(dirname(f.tmpstem),pattern=basename(f.tmpstem),full.names = TRUE)
  del.err <- unlink(tmp.fnames)
  if(del.err!=0) stop("Error deleting temp files")
  
  pb$tick()
}

#' 
#' #Tweak metadata
#' extract.meta <- select(meta.db,extract.fname) %>%
#'                 tidyr::extract(extract.fname,c("year","month"),".*.([0-9]{4})([0-9]{2}).nc",
#'                              remove=FALSE,convert=TRUE)
#' 
#' #'========================================================================
#' # Average over MOIs (if relevant) ####
#' #'========================================================================
#' #Average over time - only necessary when considering multiple target months
#' if(pcfg@average.months) {
#'   stop("Month averaging is currently unhandled in EN4. See HadISST and following code for examples.")
#'   log_msg("Monthly averaging...")
#'   
#'   #Setup MOI directories
#'   MOIave.clim.dir <- define_dir(base.dir,"C.MOIave_climatology")
#'   MOIave.anom.dir <- define_dir(base.dir,"D.MOIave_anoms")
#'   
#'   #Create a function to do this (as we need to reuse the code for
#'   #both the climatology and the anomalies)
#'   MOI.average <- function(in.src) {
#'     #monthly extraction
#'     out.fname <- gsub(".nc$","_selmon.nc",in.src)
#'     selmon.cmd <- cdo(csl("selmon",pcfg@MOI),
#'                       in.src,out.fname)
#'     
#'     #Calculate means of the anomalies
#'     in.fname <- out.fname
#'     out.fname <- gsub(".nc$","_yearmean",in.fname)
#'     yearmean.cmd <- cdo( "yearmean", in.fname,out.fname)
#'     
#'     return(out.fname)
#'   }
#'   
#'   #Now do averaging
#'   MOIave.anom <- MOI.average(remap.fname)
#'   MOIave.yearmean <- MOI.average(mon.clim.fname)
#'   
#'   #Now need to do the complete mean on MOIave.clim and move it 
#'   #to the appropriate directory
#'   MOIave.clim <- file.path(MOIave.clim.dir,"MOIave_climatology.nc")
#'   clim.cmd <- ncwa("-a time", 
#'                    MOIave.yearmean,
#'                    MOIave.clim)
#'   
#' }
#' 
#' #/*======================================================================*/
#' #  Create (pseudo) metadata 
#' #/*======================================================================*/
#' log_msg("Creating pseudo metadata...\n")
#' 
#' #Use a generic function to do the hardwork
#' generate.metadata <- function(src.dir) {
#'   #Get fnames
#'   src.fnames <- dir(src.dir,pattern=".nc",full.names = TRUE)
#'   
#'   #Extract dates
#'   meta.dat.l <- list()
#'   for(f in src.fnames) {
#'     r <- raster(f)
#'     meta.dat.l[[f]] <- tibble(date=getZ(r))
#'   }
#'   
#'   #Build metadata
#'   src.meta <- bind_rows(meta.dat.l) %>%
#'     add_column(src.name=this.src@name,
#'                src.type=this.src@type,
#'                .before=1) %>%
#'     mutate(start.date=NA,
#'            #           n.realizations=1,
#'            fname=src.fnames) 
#'   return(src.meta)
#' }
#' 
#' #Now, lets think for a minute. The downstream functions require two 
#' #files - Anomaly_metadata.RData and Realmean_metadata.RData. The choice
#' #of whether these relate to individual months or to an MOIaverage should
#' #be made here, not downstream, so we therefore need to set these up according
#' #to the project configuration. At the same time, we also want to store all
#' #metadata, so that it can be picked up later by the persistence forcast code
#' #So....
#' #First generate all monthly metadata anomalies - we need this for the persistence
#' #forecast anyway
#' mon.anom.meta <- generate.metadata(mon.anom.dir)
#' saveRDS(mon.anom.meta,file=file.path(base.dir,PE.cfg$files$Obs.monthly.anom.metadata))
#' 
#' #Now, setup rest of metadata accordingly
#' if(pcfg@average.months) {
#'   #Get metadata
#'   anom.meta <- generate.metadata(MOIave.anom.dir)
#' } else {
#'   #We are only interested in files that are in the
#'   #months of interest, so we need to filter
#'   anom.meta <- subset(mon.anom.meta,month(date) %in% pcfg@MOI)
#' }
#' 
#' #Save results and create a second copy as realmean metadata
#' saveRDS(anom.meta,file=file.path(base.dir,PE.cfg$files$anom.meta))
#' realmean.meta <- anom.meta  #Needs a rename
#' saveRDS(realmean.meta,file=file.path(base.dir,PE.cfg$files$realmean.meta))
#' 
#' #And now for the climatologies
#' if(pcfg@average.months) {
#'   #Only a single clim file - generate by hand
#'   clim.meta <- tibble(src.name=this.src@name,
#'                       src.type="Climatology",
#'                       date=as.Date(ISOdate(9999,pcfg@MOI,15)),
#'                       start.date=NA,
#'                       #                      n.realizations=1,
#'                       fname=MOIave.clim)
#'   
#' } else {
#'   #Generate a climatology 
#'   clim.meta <- generate.metadata(mon.clim.dir)
#'   clim.meta$src.type <- "Climatology"
#'   
#'   #Restrict to months in the MOI
#'   clim.meta <- subset(clim.meta,month(date) %in% pcfg@MOI)
#' }
#' saveRDS(clim.meta,file=file.path(base.dir,PE.cfg$files$Obs.climatology.metadata))

#/*======================================================================*/
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
