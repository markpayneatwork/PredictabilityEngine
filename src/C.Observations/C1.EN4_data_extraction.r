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

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(tidyverse)
pcfg <- readRDS(PE.cfg$config.path)

#/*======================================================================*/
#  Configuration
#/*======================================================================*/
#Take input arguments, if any
if(interactive()) {
  cfg.no <- 1
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
  set.nco.defaults("--overwrite")
  
} else {
  #Taking inputs from the system environment
  cfg.no <- as.numeric(Sys.getenv("LSB_JOBINDEX"))
  if(cfg.no=="") stop("Cannot find LSB_JOBINDEX")
  
  #Do everything and tell us all about it
  set.cdo.defaults()
  set.log_msg.silent(FALSE)
}

#Retrieve configurations
this.sp <- get.this.sp(file.path(PE.cfg$dirs$job.cfg,"Observations.cfg"),cfg.no,pcfg)
this.src <- pcfg@Observations
config.summary(pcfg,this.sp,this.src)

#Choose the data_src configuration
if(!this.src@name=="EN4") stop("Not configured to use EN4 data")

#'========================================================================
# Setup ####
# If we are considering looping over spatial areas in the one script, this
# is where you would start, by setting this.sp to the appropriate area
# for a list of possibilities
#'========================================================================
log_msg("\nProcessing %s...\n",this.sp@name)

#Working directories
subdomain.dir <- file.path(pcfg@scratch.dir,this.sp@name)
base.dir <- define_dir(subdomain.dir,"Observations","EN4")
extract.dir <- define_dir(base.dir,"1.extracted")
mon.clim.dir <- define_dir(base.dir,"A.monthly_climatologies")
mon.anom.dir <- define_dir(base.dir,"B.monthly_anom")
misc.meta.dir <- define_dir(base.dir,PE.cfg$dirs$Misc.meta)

tmp.dir <- tempdir()
# misc.meta.dir <- define_dir(base.dir,PE.cfg$dirs$Misc.meta)
# mon.clim.dir <- define_dir(base.dir,"A.monthly_climatologies")
# mon.anom.dir <- define_dir(base.dir,"B.monthly_anom")
# analysis.grid.fname <- file.path(subdomain.dir,PE.cfg$files$analysis.grid)

#Contents of zipfiles
#EN4.file.stem <- "EN.4.2.0.f.analysis.g10."


#/*======================================================================*/
#'## Extract EN4 meta data into fragments
#/*======================================================================*/
log_msg("Extracting metadata...\n")

#First thing to do is to get metadata of the available files, and 
#use this to define future files
meta.db <- tibble(fname=unlist(this.src@sources)) %>%
  mutate(sellev.fname=sprintf("%s.sellev.nc",fname),
         selROI.fname=sprintf("%s.selROI.nc",fname),
         tempcor.fname=sprintf("%s.degC.nc",sellev.fname),
         vertmean.fname=sprintf("%s.vertmean.nc",fname)) %>%
  mutate(extract.fname=file.path(extract.dir,basename(fname)))

#Loop over files
pb <- progress_estimated(nrow(meta.db),-1)

for(j in seq(nrow(meta.db))) {
  #For each individual file, strip out as much extra info as possible
  #by selecting the field of interest, region of interest and the layers of interest
    this.meta <- meta.db[j,]
    #Select the levels and field of interest first
    if(!length(pcfg@vert.range)==0) {
      vert.idxs <- verticalLayers(pcfg,this.src,this.meta$fname)
      sellev.cmd <- cdo(csl("sellevidx",vert.idxs),
                        csl("-selname",this.src@var),
                        this.meta$fname,
                        this.meta$sellev.fname)
    } else {
      stop("EN4 requires a vertical range specification, as the fields are 3D.")
    }

    #If we are dealing with temperature, need to conver½t from K to C
    if(this.src@var=="temperature") {
      next.fname <- this.meta$tempcor.fname
      levmean.cmd <- cdo("addc,-273.15",
                         this.meta$sellev.fname,
                         next.fname)
    } else {
      next.fname <- this.meta$sellev.fname
    }
    
    #Average over the extracted levels
    levmean.cmd <- cdo("vertmean",
                       next.fname,
                       this.meta$vertmean.fname)
    
    #Do the spatial remapping and extraction
    remap.cmd <- cdo(csl("remapbil", file.path(subdomain.dir,PE.cfg$files$analysis.grid)),
                        this.meta$vertmean.fname, 
                        this.meta$extract.fname)
  #Tidy up
  unlink(with(this.meta,c(sellev.fname,selROI.fname,vertmean.fname,tempcor.fname)))
  pb$tick()$print()
}
pb$stop()$print()

#Tweak metadata
extract.meta <- select(meta.db,extract.fname) %>%
                tidyr::extract(extract.fname,c("year","month"),".*.([0-9]{4})([0-9]{2}).nc",
                             remove=FALSE,convert=TRUE)

#/*======================================================================*/
#  Climatologies and anomalies ####
#/*======================================================================*/
#Setup climatology 
log_msg("Climatology....\n")
clim.meta <- mutate(extract.meta,
                    clim.fname=file.path(mon.clim.dir,
                                         sprintf("EN4_climatology_%02i.nc",month)))
clim.sel.dat <- filter(clim.meta,year %in% pcfg@clim.years)
clim.sel.l <- split(clim.sel.dat,clim.sel.dat$month)

pb <- progress_estimated(length(clim.sel.l),-1)

#Calculate climatologies
for(this.clim.files in clim.sel.l) {
  mon.clim.fname <-   mon.clim.cmd <- cdo("ensmean",
                      ssl(this.clim.files$extract.fname),
                      unique(this.clim.files$clim.fname))
  pb$tick()$print()
}
print(pb$stop())

#Calculate anomalies
log_msg("Anomalies...\n")
anom.meta <- mutate(clim.meta,
                    anom.fname=file.path(mon.anom.dir,basename(extract.fname)))
pb <- progress_estimated(nrow(anom.meta),-1)
for(i in seq(nrow(anom.meta))) {
  this.meta <- anom.meta[i,]
  anom.cmd <- cdo("sub",this.meta$extract.fname,this.meta$clim.fname,this.meta$anom.fname)
  pb$tick()$print()
}
print(pb$stop())

#'========================================================================
# Average over MOIs (if relevant) ####
#'========================================================================
#Average over time - only necessary when considering multiple target months
if(pcfg@average.months) {
  stop("Month averaging is currently unhandled in EN4. See HadISST and following code for examples.")
  log_msg("Monthly averaging...")
  
  #Setup MOI directories
  MOIave.clim.dir <- define_dir(base.dir,"C.MOIave_climatology")
  MOIave.anom.dir <- define_dir(base.dir,"D.MOIave_anoms")
  
  #Create a function to do this (as we need to reuse the code for
  #both the climatology and the anomalies)
  MOI.average <- function(in.src) {
    #monthly extraction
    out.fname <- gsub(".nc$","_selmon.nc",in.src)
    selmon.cmd <- cdo(csl("selmon",pcfg@MOI),
                      in.src,out.fname)
    
    #Calculate means of the anomalies
    in.fname <- out.fname
    out.fname <- gsub(".nc$","_yearmean",in.fname)
    yearmean.cmd <- cdo( "yearmean", in.fname,out.fname)
    
    return(out.fname)
  }
  
  #Now do averaging
  MOIave.anom <- MOI.average(remap.fname)
  MOIave.yearmean <- MOI.average(mon.clim.fname)
  
  #Now need to do the complete mean on MOIave.clim and move it 
  #to the appropriate directory
  MOIave.clim <- file.path(MOIave.clim.dir,"MOIave_climatology.nc")
  clim.cmd <- ncwa("-a time", 
                   MOIave.yearmean,
                   MOIave.clim)
  
}

#/*======================================================================*/
#  Create (pseudo) metadata 
#/*======================================================================*/
log_msg("Creating pseudo metadata...\n")

#Use a generic function to do the hardwork
generate.metadata <- function(src.dir) {
  #Get fnames
  src.fnames <- dir(src.dir,pattern=".nc",full.names = TRUE)
  
  #Extract dates
  meta.dat.l <- list()
  for(f in src.fnames) {
    r <- raster(f)
    meta.dat.l[[f]] <- tibble(date=getZ(r))
  }
  
  #Build metadata
  src.meta <- bind_rows(meta.dat.l) %>%
    add_column(src.name=this.src@name,
               src.type=this.src@type,
               .before=1) %>%
    mutate(start.date=NA,
           #           n.realizations=1,
           fname=src.fnames) 
  return(src.meta)
}

#Now, lets think for a minute. The downstream functions require two 
#files - Anomaly_metadata.RData and Realmean_metadata.RData. The choice
#of whether these relate to individual months or to an MOIaverage should
#be made here, not downstream, so we therefore need to set these up according
#to the project configuration. At the same time, we also want to store all
#metadata, so that it can be picked up later by the persistence forcast code
#So....
#First generate all monthly metadata anomalies - we need this for the persistence
#forecast anyway
mon.anom.meta <- generate.metadata(mon.anom.dir)
saveRDS(mon.anom.meta,file=file.path(base.dir,PE.cfg$files$Obs.monthly.anom.metadata))

#Now, setup rest of metadata accordingly
if(pcfg@average.months) {
  #Get metadata
  anom.meta <- generate.metadata(MOIave.anom.dir)
} else {
  #We are only interested in files that are in the
  #months of interest, so we need to filter
  anom.meta <- subset(mon.anom.meta,month(date) %in% pcfg@MOI)
}

#Save results and create a second copy as realmean metadata
saveRDS(anom.meta,file=file.path(base.dir,PE.cfg$files$anom.meta))
realmean.meta <- anom.meta  #Needs a rename
saveRDS(realmean.meta,file=file.path(base.dir,PE.cfg$files$realmean.meta))

#And now for the climatologies
if(pcfg@average.months) {
  #Only a single clim file - generate by hand
  clim.meta <- tibble(src.name=this.src@name,
                      src.type="Climatology",
                      date=as.Date(ISOdate(9999,pcfg@MOI,15)),
                      start.date=NA,
                      #                      n.realizations=1,
                      fname=MOIave.clim)
  
} else {
  #Generate a climatology 
  clim.meta <- generate.metadata(mon.clim.dir)
  clim.meta$src.type <- "Climatology"
  
  #Restrict to months in the MOI
  clim.meta <- subset(clim.meta,month(date) %in% pcfg@MOI)
}
saveRDS(clim.meta,file=file.path(base.dir,PE.cfg$files$Obs.climatology.metadata))

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
