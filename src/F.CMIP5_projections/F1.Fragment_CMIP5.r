#*##########################################################################*/
#' Extract data from CMIP5 projections
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Tue Sep 13 12:03:25 2016
#'
#' Extracts data from CMIP5 projections
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
cat(sprintf("\n%s\n","Extract CMIP5 data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
library(ClimateTools)
library(ncdf4)
library(dplyr)
library(tibble)
library(tidyr)
load("objects/configuration.RData")

#'========================================================================
# Configuration ####
#'========================================================================
#Take input arguments, if any
if(interactive()) {
  chunk.no <- 1
  set.debug.level(Inf)  #0 complete fresh run
  set.condexec.silent()
  set.cdo.defaults("--silent --no_warnings -O")
  set.log_msg.silent()
} else {
  #Taking inputs from the system environment
  chunk.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(chunk.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  set.debug.level(0)  #0 complete fresh run
  set.log_msg.silent(silent=FALSE)
}

#Directory setup
src.dir <- file.path("data_srcs","CMIP5")
base.dir <- define_dir(pcfg@scratch.dir,"CMIP5")
wts.dir <- define_dir(base.dir,"1.remapping_wts")
remap.dir <- define_dir(base.dir,"2.remap")
frag.dir <- define_dir(base.dir,"3.fragments")

#Split up CMIP5 job
n.CMIP5.nodes <- 20

#'========================================================================
# Setup ####
#'========================================================================
#Error checking
if(length(pcfg@MOI)!=1) stop("CMIP5 processing currently only supports single month extraction")

#Get list of files
fnames <- dir(src.dir,pattern=".nc",full.names = TRUE,recursive=TRUE)
if(length(fnames)==0) stop("Cannot find source files")

#Extract metadata
CMIP5.meta.all <- tibble(model=CMIP5_model(fnames),
                    expt=CMIP5_experiment(fnames),
                    realization=CMIP5_realisation(fnames),
                    fname=fnames)

#Check for perturbed physics runs and differing initialisation methods
#Generally, I don't know how to interpret these, so we just drop them
CMIP5.meta.all <- extract(CMIP5.meta.all,realization,
                                c("realization.r","realization.i","realization.p"),
                                "r([0-9]+)i([0-9]+)p([0-9]+)",
                                remove=FALSE)
CMIP5.meta <- subset(CMIP5.meta.all,realization.i=="1" &realization.p=="1")

#'========================================================================
# Extract data ####
#'========================================================================
#Subset the CMIP5 data
CMIP5.meta$CMIP5.chunk <- rep(1:chunk.no,length.out=nrow(CMIP5.meta))
CMIP5.meta <- subset(CMIP5.meta,CMIP5.chunk==chunk.no)

#Split into models
mdl.l <- split(CMIP5.meta,CMIP5.meta$model)

#Prepare a set of remapping weights
log_msg("Preparing weights...\n")
for(mdl in names(mdl.l)) {
  remapping.wts <- file.path(wts.dir,sprintf("%s_remapping_wts.nc",mdl))
  mdl.file.example <- subset(CMIP5.meta,model==mdl)$fname[1]
  condexec(1,wts.cmd <- cdo(csl("genbil",pcfg@analysis.grid),
                          mdl.file.example,remapping.wts))
}

#Loop over files and extract the key data from each file
log_msg("Extracting data...\n",silent=FALSE)
pb <- progress_estimated(nrow(CMIP5.meta))
for(i in seq(nrow(CMIP5.meta))) {
  pb$tick()$print()
  
  #Extract file
  cm <- CMIP5.meta[i,]
  f <- cm$fname
  temp.stem <- tempfile()
  log_msg("Fragmenting %s...\n",basename(f),silenceable = TRUE)
 
  #Subset out the surface layer from the field of interest
  #TODO: Note that we will probably need to change this in the future
  #to allow sub-surface properties - however, this is a bit of work
  #so we leave it for the moment.
  temp.in <- f
  temp.out <- sprintf("%s_sellevidx",temp.stem)
  condexec(2,sellev.cmd <- cdo("sellevidx,1",temp.in,temp.out))
  
  #Select the field of interest, just to be sure
  temp.in <- temp.out
  temp.out <- sprintf("%s_selname",temp.in)
  condexec(2,selname.cmd <- cdo(csl("selname",pcfg@CMIP5.models@var),temp.in,temp.out))

  #Select the months of interest 
  temp.in <- temp.out
  temp.out <- sprintf("%s_selmon",temp.in)
  condexec(2,selmon.cmd <- cdo(csl("selmon", pcfg@MOI),temp.in,temp.out))

  #Remap
  temp.in <- temp.out
  remap.fname <- file.path(remap.dir,basename(f))
  remap.wts <- file.path(wts.dir,
                         sprintf("%s_remapping_wts.nc",CMIP5.meta$model[i]))
  condexec(2,remap.cmd <- cdo("-f nc", 
                              csl("remap", pcfg@analysis.grid, remap.wts),
                              temp.in, remap.fname))
  
  #Remove the temporary files to tidy up
  tmp.fnames <- dir(dirname(temp.stem),pattern=basename(temp.stem),full.names = TRUE)
  del.err <- unlink(tmp.fnames)
  if(del.err!=0) stop("Error deleting temp files")
  
  #Now finally split into fragments 
  frag.prefix <- file.path(frag.dir,
                with(cm,sprintf("%s_%s_%s_",model,expt,realization)))
  condexec(3,frag.cmd <- cdo("splityear",remap.fname,frag.prefix))

}

Sys.sleep(0.1)
print(pb$stop())
log_msg("\n")

#'========================================================================
# Merge files ####
# The CMIP5 files can be broken up into more manageable chunks by the data 
# producers- however, the breaking sometimes occurs in the middle of a year, 
# rather than between years e.g. Had-CM3. This makes it hard to calculate 
# averages that cross between two files. We solve this by first merging the
# files broken into individual files for a given realisation and experiment,
# and then apply the monthly averaging to produce the fragments that 
# we are actually looking for
#
# This step is now redundant due to the fact that we are fragmenting to 2D
# fields.
#'========================================================================
# log_msg("Merging files...\n")


# #Setup filenames
# src.meta.df$remap.fname <- file.path(remap.dir,basename(src.meta.df$fname))
# src.meta.df$yearmean.fname <- with(src.meta.df,
#                                 sprintf("%s_%s_%s.nc",model,expt,real))
# src.meta.df$yearmean.fname <- file.path(yearmean.dir,src.meta.df$yearmean.fname)
# src.meta.df$realmean.fname <- with(src.meta.df,
#                                    sprintf("%s_%s_realmean.nc",model,expt))
# src.meta.df$realmean.fname <- file.path(realmean.dir,src.meta.df$realmean.fname)

# Split into individual experiments and realisations
# yearmean.l <- split(CMIP5.meta,
#                     CMIP5.meta[,c("model","expt","realization")],drop=TRUE)
# 
# #Now loop over the merging groups
# pb <- progress_estimated(length(yearmean.l))
# for(ym in yearmean.l){
#   pb$tick()$print()
#   
#   #Merge into single files
#   remap.fnames <- file.path(remap.dir,basename(ym$fname))
#   merge.tmp.fname <- tempfile(fileext="_mergetime.nc")
#   merge.cmd <- cdo("-O mergetime",remap.fnames,merge.tmp.fname)
#   condexec(3,merge.cmd)
#   
#   #Now perform averaging over the month
#   frag.fname <- file.path(frag.dir,
#                           with(ym[1,],sprintf("%s_%s_%s.nc",model,expt,realization)))
#   condexec(3,yearmean.cmd <- cdo("yearmean", merge.tmp.fname,frag.fname))
#   unlink(merge.tmp.fname)
# }
# 
# pb$stop()
# print(pb)
# rm(pb)
# log_msg("\n")
# 

#'========================================================================
# Complete ####
#'========================================================================
#Save the metadata
save(CMIP5.meta,CMIP5.meta.all,file=file.path(base.dir,"CMIP5files_metadata.RData"))

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
