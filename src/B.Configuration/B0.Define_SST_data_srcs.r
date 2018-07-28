#/*##########################################################################*/
#' Define SST Data Sources
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Sep  1 19:17:39 2016
#'
#' Defines the Set of supported data sources using for examining SST predictability
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
# Setup hindcast models
# ========================================================================
#IPSL and MPI-MR have basically an identifical structure, both being produced
#originally by SPECS
hindcast_mdls <- PredEng.list()
hindcast_mdls$IPSL  <- data.source(name="IPSL-CM5A-LR",
                                   type="Decadal",
                           source="IPSL-CM5A-LR",
                           var="tos",
                           ensmem_fn=function(f) {
                             underscore_field(f,6)},
                           init_fn=function(f){
                             init.str <- gsub("S","",underscore_field(f,5))
                             init.date <- ymd(init.str)
                             return(init.date)})

hindcast_mdls$"MPI-MR" <-  new("data.source",hindcast_mdls$IPSL,
                               name="MPI-ESM-MR",
                               source="MPI-ESM-MR")

#MPI-LR is different,
hindcast_mdls$"MPI-LR" <-  data.source(name="MPI-ESM-LR",
                                       type="Decadal",
                                       var="thetao",
                               source="MPI-ESM-LR_MiKlip-b1",
                             ensmem_fn=CMIP5_realisation,
                             init_fn=function(f){
                               init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                               init.date <- ymd(paste(init.str,"01",sep=""))
                               return(init.date)})

#MPI - NCEP requires all three to be specified
hindcast_mdls$"MPI-NCEP" <- data.source(name="MPI-NCEP-forced",
                                type="Decadal",
                                source="MPI-ESM-LR_NCEP-forced",
                                var="var2",
                             ensmem_fn=function(f){return(rep("r1",length(f)))},
                             date_fn=function(f){
                               dates.str <- getZ(brick(f))
                               dates <- ymd(dates.str)
                               day(dates) <- 15  #Adjust to be mid month, rather than end of month
                               #But adjust
                               return(dates)},
                             init_fn=function(f){
                               init.yr <- str_match(basename(f),"^dma([0-9]{4})_.*$")[,2]
                               init.date <- as.Date(ISOdate(init.yr,1,1))
                               return(init.date)})

#GFDL is largely in CMIP5 format
hindcast_mdls$GFDL <-   data.source(name="GFDL-CM2.1",
                            type="Decadal",
                            source="GFDL-CM2.1",
                            var="tos",
                             ensmem_fn=CMIP5_realisation,
                             init_fn=function(f){
                               init.yr <- str_match(basename(f),"^.*?_decadal([0-9]{4})_r.*$")[,2]
                               init.date <- as.Date(ISOdate(init.yr,1,1))
                               return(init.date)})

#Identify models as hindcast models and set the source equal to the name
for(i in seq(hindcast_mdls)) {
  hindcast_mdls[[i]]@type <- "Decadal"
}

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
SST_obs <- PredEng.list()
SST_obs$HadISST <- data.source(name="HadISST",var="sst")
SST_obs$OISST <- data.source(name="OISST")
SST_obs$EN4  <- data.source(name="EN4",var="temperature")
for(i in seq(SST_obs)){
  SST_obs[[i]]@source <- file.path("Observations",SST_obs[[i]]@name)
  SST_obs[[i]]@type <- "Observations"
}

# ========================================================================
# Setup NMME models
# ========================================================================
library(readr)
NMME.cfg <- read_csv2(file.path(PE.cfg$datasrc.dir,"NMME","NMME_SST_urls.csv"))
NMME.mdls <- split(NMME.cfg,NMME.cfg$Model)
NMME.sst.l <- PredEng.list()
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
NMME.sst.l[["NCEP-CFSv2"]]@realizations <- 1:24  #Forecast has 32 but hindcast 24. Restrict to be the same for simplicity

# ========================================================================
# Setup CMIP5 models
# ========================================================================
CMIP5.mdls <- data.source(name="CMIP5-tos",type="CMIP5",var="tos",
                  source="CMIP5")
  
