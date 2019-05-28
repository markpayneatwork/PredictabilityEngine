#/*##########################################################################*/
#' Define Data Sources
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Sep  1 19:17:39 2016
#'
#' Defines a set of commonly employed data sources 
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
# Helper functions ####
#'========================================================================
date.by.brick <- function(f) {
  if(length(f)>1) stop("Function not vectorised")
  dates <- getZ(brick(f))
  return(dates)}

decadal.dir <- file.path(PE.cfg$dirs$datasrc,"Decadal")

# ========================================================================
# Setup SST.Decadal models
# ========================================================================
#IPSL and MPI-MR have basically an identifical structure, both being produced
#originally by SPECS
SST.Decadal <- list()
SST.Decadal$IPSL  <- data.source(name="IPSL-CM5A-LR",
                                 type="Decadal",
                                 source=dir(file.path(decadal.dir,"IPSL-CM5A-LR"),
                                            pattern="\\.nc$",full.names = TRUE),
                                 var="tos",
                                 ensmem.fn=function(f) {
                                   underscore_field(f,6)},
                                 init.fn=function(f){
                                   init.str <- gsub("S","",underscore_field(f,5))
                                   init.date <- ymd(init.str)
                                   return(init.date)},
                                 date.fn=date.by.brick)

SST.Decadal$"MPI-MR" <-  new("data.source",
                             SST.Decadal$IPSL,
                             name="MPI-ESM-MR",
                             source=dir(file.path(decadal.dir,"MPI-ESM-MR"),
                                        pattern="\\.nc$",full.names = TRUE))

#MPI-LR is different,
SST.Decadal$"MPI-LR" <-  data.source(name="MPI-ESM-LR",
                                     type="Decadal",
                                     var="thetao",
                                     source=dir(file.path(decadal.dir,"MPI-ESM-LR_MiKlip-b1","thetao"),
                                                pattern="\\.nc$",full.names = TRUE),
                                     ensmem.fn=CMIP5_realisation,
                                     init.fn=function(f){
                                       init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                                       init.date <- ymd(paste(init.str,"01",sep=""))
                                       return(init.date)},
                                     date.fn=date.by.brick)

#MPI - NCEP requires all three to be specified
SST.Decadal$"MPI-NCEP" <- data.source(name="MPI-NCEP-forced",
                                      type="Decadal",
                                      source=dir(file.path(decadal.dir,"MPI-ESM-LR_NCEP-forced"),
                                                 pattern="\\.nc$",full.names = TRUE),
                                      var="var2",
                                      ensmem.fn=function(f){return(rep("r1",length(f)))},
                                      date.fn=function(f){
                                        dates.str <- getZ(brick(f))
                                        dates <- ymd(dates.str)
                                        day(dates) <- 15  #Adjust to be mid month, rather than end of month
                                        #But adjust
                                        return(dates)},
                                      init.fn=function(f){
                                        init.yr <- str_match(basename(f),"^dma([0-9]{4})_.*$")[,2]
                                        init.date <- as.Date(ISOdate(init.yr,1,1))
                                        return(init.date)})

#GFDL is largely in CMIP5 format
SST.Decadal$GFDL <-   data.source(name="GFDL-CM2.1",
                                  type="Decadal",
                                  source=dir(file.path(decadal.dir,"GFDL-CM2.1"),
                                             pattern="\\.nc$",full.names = TRUE),
                                  var="tos",
                                  ensmem.fn=CMIP5_realisation,
                                  init.fn=function(f){
                                    init.yr <- str_match(basename(f),"^.*?_decadal([0-9]{4})_r.*$")[,2]
                                    init.date <- as.Date(ISOdate(init.yr,1,1))
                                    return(init.date)},
                                  date.fn=date.by.brick)

#Add in the CESM DPLE
#This could be split into multiple sub-sources, but then that starts to create problems when
#it comes time to calculate the ensemble mean etc. We need to think that one through a bit....
SST.Decadal$CESM.DPLE <-   data.source(name="CESM-DPLE",
                               type="Decadal",
                               source=dir(file.path(PE.cfg$dirs$datasrc,"Decadal","CESM-DPLE","SST"),
                                          pattern="\\.nc$",full.names = TRUE),
                               levels=as.numeric(NA),
                               var="SST",
                               time.correction="-15days",
                               ensmem.fn=function(f) {
                                 val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,3]
                                 return(val)},
                               init.fn=function(f) {
                                 val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,2]
                                 return(ymd(sprintf("%s-01",val)))},
                               date.fn=date.by.brick)

#Set list names
names(SST.Decadal) <- sapply(SST.Decadal,slot,"name")


#'========================================================================
# Salinity data sources ####
#'========================================================================
Sal.Decadal <- list()
Sal.Decadal$"MPI-LR" <-  data.source(name="MPI-ESM-LR",
                                     type="Decadal",
                                     var="so",
                                     source=dir(file.path(decadal.dir,"MPI-ESM-LR_MiKlip-b1","so"),
                                                pattern="\\.nc$",full.names = TRUE),
                                     ensmem.fn=CMIP5_realisation,
                                     init.fn=function(f){
                                       init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                                       init.date <- ymd(paste(init.str,"01",sep=""))
                                       return(init.date)},
                                     date.fn=date.by.brick)

#Set list names
names(Sal.Decadal) <- sapply(Sal.Decadal,slot,"name")


# ========================================================================
# Setup uninitialised models
# These should largely be the same...
# ========================================================================
# uninit_mdls <- hindcast_mdls
# 
# #As these are uninitalised runs, the idea of a initialisation date doesn't
# #make much sense, so we set all init_fn to NA
# for(i in seq(uninit_mdls)) {
#   uninit_mdls[[i]]@init_fn <- function(f) {rep(NA,length(f))}
# }
# 
# #MPI-NCEP-forced doesn't have any uninitialised runs
# uninit_mdls$`MPI-NCEP` <- NULL
# 
# 
# #For all other models, we use the standard CMIP5 products
# for(i in seq(uninit_mdls)) {
#   uninit_mdls[[i]]@ensmem_fn <- CMIP5_realisation
#   uninit_mdls[[i]]@var <- "tos"
#   uninit_mdls[[i]]@type <- "uninit"
# }

# ========================================================================
# Setup observational data sets
# ========================================================================
SST_obs <- list()
SST_obs$HadISST <- data.source(name="HadISST",
                               type="Observations",
                               var="sst",
                               source="data_srcs/Observations/HadISST/HadISST_sst.nc")
SST_obs$EN4  <- data.source(name="EN4",
                            type="Observations",
                            var="temperature",
                            source=dir("data_srcs/Observations/EN4/",
                                       pattern="\\.zip$",full.names = TRUE))

# ========================================================================
# Setup NMME models
# ========================================================================
library(readr)
NMME.cfg <- read_csv2(file.path(PE.cfg$dirs$datasrc,"NMME","NMME_SST_urls.csv"))
NMME.mdls <- split(NMME.cfg,NMME.cfg$Model)
NMME.sst.l <- list()
for(mdl.name in names(NMME.mdls)){
  mdl <- NMME.mdls[[mdl.name]]
  mdl.src <- mdl$URL
  names(mdl.src) <- mdl$type
  obj <- data.source(name=mdl.name,#sprintf("NMME-%s",mdl.name),
                     type="NMME",
                     var="sst",
                     source=mdl.src)  
  NMME.sst.l[[mdl.name]] <- obj
}

#Restrict some realisations
NMME.sst.l[["NASA-GEOS5"]]@realizations <- 1:11  #12th realization is very intermittant
NMME.sst.l[["NCEP-CFSv2"]]@realizations <- 1:24  #Forecast has 32 but hindcast 24. 

#Restrict to be the same for simplicity
#'========================================================================
# Setup CMIP5 models ####
#'========================================================================
#Get list of files
CMIP5.fnames <- dir(file.path("data_srcs","CMIP5"),pattern=".nc",full.names = TRUE,recursive=TRUE)
if(length(CMIP5.fnames)==0) stop("Cannot find source files")

#Extract metadata
CMIP5.meta.all <- tibble(model=CMIP5_model(CMIP5.fnames),
                         expt=CMIP5_experiment(CMIP5.fnames),
                         realization=CMIP5_realisation(CMIP5.fnames),
                         fname=CMIP5.fnames)

#Check for perturbed physics runs and differing initialisation methods
#Generally, I don't know how to interpret these, so we just drop them
CMIP5.meta.all <- tidyr::extract(CMIP5.meta.all,realization,
                                 c("realization.r","realization.i","realization.p"),
                                 "r([0-9]+)i([0-9]+)p([0-9]+)",
                                 remove=FALSE)
CMIP5.meta <- subset(CMIP5.meta.all,realization.i=="1" &realization.p=="1")

#Split the CMIP5 data into chunks
CMIP5.meta$CMIP5.chunk <- as.numeric(factor(CMIP5.meta$model)) %% PE.cfg$n.CMIP.chunks
CMIP5.grp <- split(CMIP5.meta,CMIP5.meta$CMIP5.chunk)

#Now create the PredEng.source objects
CMIP5.mdls.l <- list()
for(i in seq(CMIP5.grp)) {
  CMIP5.mdls.l[[i]] <- data.source(name=sprintf("Chunk %03i",i),
                                   type="CMIP5",
                                   var="tos",
                                   source=CMIP5.grp[[i]]$fname)
}
names(CMIP5.mdls.l) <- sapply(CMIP5.mdls.l,slot,"name")


