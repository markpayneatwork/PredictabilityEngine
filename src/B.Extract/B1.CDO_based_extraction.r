#/*##########################################################################*/
#' Extract data using CDO
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Jun  2 15:10:05 2016
#'
#' Extracts data from CDO-compatible CMOR-ised outputs
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
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","Extract CDO-compatible data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
suppressPackageStartupMessages({
  library(PredEng)
  library(furrr)
  library(pbapply)
  #library(parallel)
})
pcfg <- PE.load.config()

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive() ) {
  set.cdo.defaults("--silent --no_warnings -O -f nc4")
  set.log_msg.silent()
  this.srcType <- pcfg@data.sources[[1]]@type
  this.srcName <-  pcfg@data.sources[[1]]@name
  # this.srcType <- "Observations"
  # this.srcName <- "ORAS5"
} else {  
  #Running as a terminal
  cmd.args <- commandArgs(TRUE)
  assert_that(length(cmd.args)==2,msg="Cannot get command args")
  this.srcType <- cmd.args[1]
  this.srcName <- cmd.args[2]
  set.cdo.defaults("--silent --no_warnings -O -f nc4 ")
  set.log_msg.silent()
}

#Setup parallelism
if(Sys.info()["nodename"]=="aqua-cb-mpay18" | interactive()) {
  n.cores <- 8
} else if(Sys.info()["nodename"]=="volta.dmi.dk" ) {
  n.cores <- 8
} else {
  n.cores <- as.numeric(Sys.getenv("LSB_DJOB_NUMPROC"))    
  assert_that(!is.na(n.cores),msg = "Cannot detect number of allocated cores")
}
plan(multisession,workers = n.cores)

#Other configurations
set.nco.defaults("--quench --overwrite")

#Get data source
this.datasrc <- PE.get.datasrc(pcfg,this.srcType,this.srcName)
this.extract.tbl <- case_when(this.srcType=="Observations"~PE.cfg$db$calibration,
                              TRUE ~ PE.cfg$db$extract)

#Display configuration
PE.config.summary(pcfg,this.datasrc)

#'========================================================================
# Setup ####
#'========================================================================
#Setup
#Note that we preassign tempfile filenames. This is probably not necessary,
#but avoids the risk of duplication when we are dealing with parallelisation
these.srcs <- 
  tibble(src.fname=this.datasrc@sources,
         tmp.stem=tempfile(fileext = rep("",length(src.fname))))

#Check configuration is sane
assert_that(nrow(these.srcs)>0,msg="No source files provided")
assert_that(all(file.exists(these.srcs$src.fname)),msg="Cannot find all source files")

#Setup extraction database and delete all previous entries of this datasource
#My fear is that
#an interruption of the source processing may lead to only a subset of
#the fragments being produced from a given file. Hence, required that
#the datasource extraction is run in one large chunk all the way to completion.
PE.db.setup.extraction(pcfg,this.datasrc)  
PE.db.delete.by.datasource(pcfg,PE.cfg$db$extract,this.datasrc)

#'========================================================================
# Extract Fragments from Source Files ####
#'========================================================================
extract.frags <- function(src.fname,tmp.stem,opts) {
  # src.fname <- these.srcs[2,]$src.fname
  # tmp.stem <- these.srcs[2,]$tmp.stem
  #Get started
  #The use of generic tmp.in and tmp.out filenames allows the order of 
  #execution to be reshuffled as required
  options(opts)
  if(file.exists(tmp.stem)) file.remove(tmp.stem)
  file.symlink(src.fname,tmp.stem)
  tmp.out <- tmp.stem

  #Before selecting the months of interest, we may need to apply a time
  #correction of the time axis. CESM-DPLE, for example, has the time axis
  #set to 2018-08-01 to represent the period 2018-07-01-2018-08-01, meaning
  #that is actually the average value for July, but is labelled as August. It's a trap!
  #This is where we correct for that effect by copying the time bounds into the
  #time variable, and therefore ensuring that e.g. selmon works properly
  if(!is.na(this.datasrc@use.timebounds)) {
    timebounds.to.time(this.datasrc,tmp.out)
  }
  
  #Check that we actually have the Month of Interest in the file. If not, bail
  these.months <- this.datasrc@date.fn(tmp.out)
  if(!any(month(these.months) %in% pcfg@MOI)) {return(NULL)}
  
  #Select the months of interest 
  tmp.in <- tmp.out
  tmp.out <- sprintf("%s_selmon",tmp.in)
  selmon.cmd <- cdo(csl("selmon", pcfg@MOI),tmp.in,tmp.out)
  
  #Average over time - only necessary when considering multiple target months
  if(pcfg@average.months) {
    tmp.in <- tmp.out
    tmp.out <- sprintf("%s_yearmean",tmp.in)
    yearmean.cmd <- cdo( "yearmean", tmp.in,tmp.out)
  }

  #Subset out the layer(s) from the field of interest, if relevant
  if(!this.datasrc@fields.are.2D ) { #Field is 3D and requires selection
    tmp.in <- tmp.out
    tmp.out <- sprintf("%s_sellevel",tmp.in)
    if(length(pcfg@vert.range)==1 & all(is.na(pcfg@vert.range))) { 
      #Then we are requesting the surface layer only
      vert.idxs <- 1
    } else {
      #Need to work it out ourselves
      vert.idxs <- this.datasrc@z2idx(pcfg@vert.range,tmp.in)
    }
    sellev.cmd <- cdo(csl("sellevidx",vert.idxs),
                      tmp.in,tmp.out)
  }

  #Average over the layers
  tmp.in <- tmp.out
  tmp.out <- sprintf("%s_vertmean",tmp.in)
  levmean.cmd <- cdo("vertmean",tmp.in,tmp.out)

  #Select the field of interest, just to be sure
  tmp.in <- tmp.out
  tmp.out <- sprintf("%s_selname",tmp.in)
  selname.cmd <- cdo(csl("selname",this.datasrc@var),tmp.in,tmp.out)
  
  #Remap recalculating weights every time
  analysis.grid.fname <- PE.scratch.path(pcfg,PE.cfg$file$analysis.grid)
  tmp.in <- tmp.out
  tmp.out <- sprintf("%s_regrid",tmp.in)
  regrid.cmd <- cdo("-f nc",
                    csl("remapbil", analysis.grid.fname),
                    tmp.in, tmp.out)
  

  #Import data into raster-land
  dat.b <- readAll(brick(tmp.out))

  #Set CRS status
  #As everything is interpolated onto a common grid, it should also therefore
  #have a CRS reflecting that grid.
  dat.b@crs <- PE.cfg$misc$crs
  
  #Create metadata
  this.frag <- tibble(srcFname=basename(src.fname),
                      srcName=this.datasrc@name,
                      srcType=this.datasrc@type,
                      realization=this.datasrc@realization.fn(src.fname),
                      startDate=this.datasrc@start.date(src.fname),
                      date=this.datasrc@date.fn(tmp.out),
                      lead=month_diff(date,startDate),
                      field=as.list(dat.b))

  #Remove the temporary files to tidy up
  tmp.fnames <- dir(dirname(tmp.stem),pattern=basename(tmp.stem),full.names = TRUE)
  del.err <- unlink(tmp.fnames)
  assert_that(del.err==0,msg="Error deleting temp files")
  
  #Return
  return(this.frag)
}

#Sanity check
log_msg("Dry run...\n")
san.chk <- system.time(extract.frags(these.srcs[1,]$src.fname,
                                     these.srcs[1,]$tmp.stem,
                                     opts=options("ClimateOperators")))
log_msg("Passed in %2.2fs.\n",san.chk[3])

#'========================================================================
# Apply extraction ####
#'========================================================================
#' To avoid conflicts with too many processes trying to write to the database
#' at the same time, we batch the process up into chunks of approximately 100
#' files at a time, and then use a parallelised apply process
#Loop over Source Files
log_msg("Extracting fragments from source files using %i cores...\n",n.cores)

chunk.l <- 
  these.srcs %>%
  mutate(batch.id=rep(seq(nrow(.)),
                      each=n.cores,
                      length.out=nrow(.))) %>%
  group_by(batch.id) %>%
  group_split(.keep=FALSE)

#Now loop over the chunks in a parallelised manner
pb <- PE.progress(length(chunk.l))
dmp <- pb$tick(0)
for(this.chunk in chunk.l) {
  #Using Furrr and future
  frag.dat <-
    future_pmap_dfr(this.chunk,
                    extract.frags,
                    opts=options("ClimateOperators"),
                    .options = furrr_options(stdout=FALSE,
                                             seed=TRUE))
  
  #Using purr (serialised)
  #frag.dat <-
  #  pmap_dfr(this.chunk,
  #                  extract.frags)
  
  #Using mcmapply (parallel / multicore)
  # frag.dat <-
  #	mcmapply(extract.frags,
  #               this.chunk$src.fname,this.chunk$tmp.stem,
  #               SIMPLIFY=FALSE,
  #               mc.silent=TRUE,
  #               mc.cores=n.cores) %>%
  #      bind_rows()

  #Write to database
  frag.dat %>%
    mutate(startDate=as.character(startDate),
           date=as.character(date)) %>%
    PE.db.appendTable(pcfg,PE.cfg$db$extract,this.datasrc,dat=.)
  
  #Loop
  pb$tick()
}

#'========================================================================
# Complete 
#'========================================================================
#Turn off thte lights
plan(sequential)
if(length(warnings())!=0) print(warnings())
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
