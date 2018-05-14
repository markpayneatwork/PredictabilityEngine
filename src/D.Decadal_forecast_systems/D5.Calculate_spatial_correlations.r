#/*##########################################################################*/
#' Calculate Spatial Correlation Skill metrics
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Aug 25 14:47:21 2016
#'
#' Calculates skill metrics in a spatial manner
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

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Calculate spatial correlation skill metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Configure markdown style, do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
library(PredEng)
load("objects/configuration.RData")
library(reshape2)

# ========================================================================
# Configuration
# ========================================================================
#Take input arguments, if any
if(interactive()) {
  mdl.no <- 0
  options("run.level"= 0)  #0 complete fresh run
} else {
  #Taking inputs from the system environment
  mdl.no <- as.numeric(Sys.getenv("PBS_ARRAYID"))
  if(mdl.no=="") stop("Cannot find PBS_ARRAYID")
  #Do everything
  options("run.level"= 0)  #0 complete fresh run
}

#Directory setup
base.dir <- define_dir("processing",pcfg@name)
cor.dir <- define_dir(base.dir,"correlation_skill")
obs.fname <- file.path(base.dir,pcfg@observations[[1]]@name,"observations.nc")

# ========================================================================
# Setup 
# ========================================================================
if(length(pcfg@hindcast.models)>1) {
  mdl.list <- c(GCM(name="Ensmean",type="hindcast"),pcfg@hindcast.models)
} else {
  mdl.list <- c(pcfg@hindcast.models)
  
}

for(mdl in mdl.list){
  
#load Meta data
load(file.path(base.dir,sprintf("%s-%s",mdl@name,mdl@type),"metadata.RData"))

# Extract realisation means from meta data
meta.df$fperiod.start <- date(ISOdate(year(meta.df$forecast.date),min(pcfg@MOI),1))
realmean.meta <- subset(meta.df,real%in%c("realmean","ensmean"))

#Restrict to the years over which are comparing
in.clim.meta <- subset(realmean.meta,year(fperiod.start) %in% pcfg@comp.years)

#Break into chunks according to lead time
in.clim.meta$lead.years <- round(as.numeric(difftime(in.clim.meta$fperiod.start,in.clim.meta$forecast.init,
                              units="days"))/365,1)
lead.l <- split(in.clim.meta,in.clim.meta$lead.years)

# ========================================================================
# Calculate correlation maps
# ========================================================================
#Loop over lead times
for(i in seq(lead.l)){
    log_msg("Model %s, Lead %i...\n",mdl@name,i)
    l <- lead.l[[i]]
    anom.fnames <- l$fname
    #First merge files by time
    anom.merge.fname <- tempfile(fileext=".nc")
    run_if(1,merge.cmd <- cdo("mergetime",anom.fnames,anom.merge.fname))
    #Subset observations to match
    obs.subset.fname <- tempfile(fileext="_obssubset.nc")
    run_if(2,obs.cmd <- cdo(csl("selyear",year(l$forecast.date)),
                            obs.fname,obs.subset.fname))
    #Generate correlation maps
    cor.map.fname <- file.path(tempdir(),
                               sprintf("%s_Cor_map_lead_%02i.nc",mdl@name,i))
    run_if(3,cor.cmd <- cdo("timcor",
                             obs.subset.fname,anom.merge.fname,cor.map.fname))
}

#Merge into one file
cor.fnames <- dir(tempdir(),pattern=sprintf("%s_Cor_map_lead*",mdl@name),
                  full.names = TRUE)
run_if(3,ncrcat("-D 1 -O -o",
                file.path(cor.dir,sprintf("%s_cor_map.nc",mdl@name)),
                cor.fnames))

} #End model loop

# ========================================================================
# Complete
# ========================================================================
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
