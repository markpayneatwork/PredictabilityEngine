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
date.by.brick <- 
  function(f,varname="") {
    if(length(f)>1) stop("Function not vectorised")
    dates <- getZ(brick(f,varname=varname))
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
                                 sources=list(dir(file.path(decadal.dir,"IPSL-CM5A-LR"),
                                                  pattern="\\.nc$",full.names = TRUE)),
                                 var="tos",
                                 realization.fn=function(f) {
                                   underscore_field(f,6)},
                                 start.date=function(f){
                                   init.str <- gsub("S","",underscore_field(f,5))
                                   init.date <- ymd(init.str)
                                   return(init.date)},
                                 date.fn=date.by.brick)

SST.Decadal$"MPI-MR" <-  new("data.source",
                             SST.Decadal$IPSL,
                             name="MPI-ESM-MR",
                             sources=list(dir(file.path(decadal.dir,"MPI-ESM-MR"),
                                              pattern="\\.nc$",full.names = TRUE)))

#MPI-LR is different,
SST.Decadal$"MPI-LR" <-  
  data.source(name="MPI-ESM-LR",
              type="Decadal",
              var="thetao",
              sources=list(dir(file.path(decadal.dir,"MPI-ESM-LR_MiKlip-b1","thetao"),
                               pattern="\\.nc$",full.names = TRUE)),
              realization.fn=CMIP5_realisation,
              start.date=function(f){
                init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                init.date <- ymd(paste(init.str,"01",sep=""))
                return(init.date)},
              start.id=function(f){
                init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                init.date <- ymd(paste(init.str,"01",sep=""))
                init.id <- year(init.date)
                return(init.id)},
              date.fn=date.by.brick) %>%
  chunk.data.source(n=16)  #Around 15 min runtime for Bluefin

#MPI - NCEP requires all three to be specified
SST.Decadal$"MPI-NCEP" <- data.source(name="MPI-NCEP-forced",
                                      type="Decadal",
                                      sources=list(dir(file.path(decadal.dir,"MPI-ESM-LR_NCEP-forced"),
                                                       pattern="\\.nc$",full.names = TRUE)),
                                      var="var2",
                                      realization.fn=function(f){return(rep("r1",length(f)))},
                                      date.fn=function(f){
                                        dates.str <- getZ(brick(f))
                                        dates <- ymd(dates.str)
                                        day(dates) <- 15  #Adjust to be mid month, rather than end of month
                                        #But adjust
                                        return(dates)},
                                      start.date=function(f){
                                        init.yr <- str_match(basename(f),"^dma([0-9]{4})_.*$")[,2]
                                        init.date <- as.Date(ISOdate(init.yr,1,1))
                                        return(init.date)})

#GFDL is largely in CMIP5 format
SST.Decadal$GFDL <-   data.source(name="GFDL-CM2.1",
                                  type="Decadal",
                                  sources=list(dir(file.path(decadal.dir,"GFDL-CM2.1"),
                                                   pattern="\\.nc$",full.names = TRUE)),
                                  var="tos",
                                  realization.fn=CMIP5_realisation,
                                  start.date=function(f){
                                    init.yr <- str_match(basename(f),"^.*?_decadal([0-9]{4})_r.*$")[,2]
                                    init.date <- as.Date(ISOdate(init.yr,1,1))
                                    return(init.date)},
                                  date.fn=date.by.brick)

#Add in the CESM DPLE
CESM.DPLE.src <-   
  data.source(name="CESM-DPLE",
              var="SST",
              type="Decadal",
              sources=list(dir(file.path(PE.cfg$dirs$datasrc,"Decadal","CESM-DPLE","SST"),
                               pattern="\\.nc$",full.names = TRUE)),
              time.correction="-15days",
              realization.fn=function(f) {
                val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,3]
                return(val)},
              start.date=function(f) {
                val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,2]
                return(ymd(sprintf("%s-01",val)))},
              start.id=function(f){
                val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,2]
                start.date <- ymd(sprintf("%s-01",val))
                return(year(start.date)+1)},  #November start - round up
              date.fn=date.by.brick) %>%
  chunk.data.source(n=22)  #~15 min runtime for Bluefin

SST.Decadal$CESM.DPLE <- CESM.DPLE.src

#NorCPM
#Note that there are two initialisations here, stored in the same directory - i1 and i2.
#We treat them as different data sources for the purpose of this analysis
NorCPM.fnames <- 
  tibble(path=dir(file.path(PE.cfg$dirs$datasrc,"Decadal","NorCPM"),recursive = TRUE,full.names = TRUE),
         fname=basename(path)) %>%
  separate(fname,into=c("field","table","model","experiment","variant","grid","time"),sep="_") %>%
  separate(variant,into=c("start","realization"),sep="-") %>%
  extract(realization,c("realization","initialization","other"),"^(r[[:digit:]]+)(i[[:digit:]]+)(.+)$")

NorCPM.SST.src.i1 <- 
  data.source(name="NorCPM.i1",
              var="tos",
              type="Decadal",
              sources=list(filter(NorCPM.fnames,
                             field=="tos",
                             initialization=="i1")$path),
              realization.fn = function(f) {
                gsub("^.*_s[[:digit:]]{4}-r([[:digit:]]+)i.*$","\\1",basename(f))},
              start.date=function(f){
                ymd(gsub("^([[:digit:]]{6})-.*$","\\101",underscore_field(basename(f),7)))},
              start.id=function(f) {
                val <- gsub("^([[:digit:]]{6})-.*$","\\101",underscore_field(basename(f),7))
                return(year(ymd(val))+1)},
              date.fn=function(f) {
                  if(length(f)>1) stop("Function not vectorised")
                  dates <- getZ(brick(f,varname="tos"))
                  return(dates)}) %>%
  chunk.data.source(n=10) 

NorCPM.SST.src.i2 <- 
  new("data.source",
      NorCPM.SST.src.i1,
      name="NorCPM.i2",
      sources=list(filter(NorCPM.fnames,
                          field=="tos",
                          initialization=="i2")$path)) %>%
  chunk.data.source(n=10) 

SST.Decadal <- c(SST.Decadal,NorCPM.SST.src.i1,NorCPM.SST.src.i2)

#Set list names and ids
names(SST.Decadal) <- sapply(SST.Decadal,slot,"name")

SST.Decadal.production <- SST.Decadal[c("CESM-DPLE","MPI-ESM-LR","NorCPM.i1","NorCPM.i2")]

#'========================================================================
# Salinity data sources ####
#'========================================================================
Sal.Decadal <- list()

#MPI-LR
#There is a problem with the date-time stamps on the first realisation in this hindcast
#ensemble. It could be corrected, but to start with we just drop it, awaiting the updated
#data set from Daniela. 2019.06.04
# MPI.LR.srcs <- dir(file.path(decadal.dir,"MPI-ESM-LR_MiKlip-b1","so"),
#                    pattern="\\.nc$",full.names = TRUE) %>%
#                 subset(!grepl("r1i1p1",.))
#Received the corrected files and the full 10 member ensemble, so this should work properly now
MPI.LR.srcs <- dir(file.path(decadal.dir,"MPI-ESM-LR_MiKlip-b1","so_10member"),
                   pattern="\\.nc$",full.names = TRUE) 

Sal.Decadal$"MPI-LR" <-  
  data.source(name="MPI-ESM-LR",
              type="Decadal",
              var="so",
              sources=list(MPI.LR.srcs),
              realization.fn=CMIP5_realisation,
              start.date=function(f){
                init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                init.date <- ymd(paste(init.str,"01",sep=""))
                return(init.date)},
              start.id=function(f){
                init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                init.date <- ymd(paste(init.str,"01",sep=""))
                init.id <- year(init.date)
                return(init.id)},
              date.fn=date.by.brick) %>%
  chunk.data.source(n=5)

#CESM-DPLE
CESM.DPLE.SALT <- 
  data.source(CESM.DPLE.src,
              var="SALT",
              sources=list(dir(file.path(PE.cfg$dirs$datasrc,"Decadal","CESM-DPLE","SALT"),
                               pattern="\\.nc$",full.names = TRUE)),
              layermids.fn = function(f) {
                #z.idx are the indices, v is the vertical coordinate in metres
                ncid <- nc_open(f)
                layer.mids <- ncid$dim$z_t$vals/100 #[m]
                nc_close(ncid)
                return(layer.mids)},
              start.id=function(f){
                val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,2]
                start.date <- ymd(sprintf("%s-01",val))
                return(year(start.date)+1)}) %>%  #November start - round up
  chunk.data.source(n=20)

Sal.Decadal$CESM.DPLE.SALT <- CESM.DPLE.SALT

NorCPM.sal.src.i1 <- 
  new("data.source",
      var="so",
      NorCPM.SST.src.i1,
      name="NorCPM.i1",
      sources=list(filter(NorCPM.fnames,
                          field=="so",
                          initialization=="i1")$path)) %>%
  chunk.data.source(n=10) 

NorCPM.sal.src.i2 <- 
  new("data.source",
      NorCPM.SST.src.i1,
      name="NorCPM.i2",
      sources=list(filter(NorCPM.fnames,
                          field=="so",
                          initialization=="i2")$path)) %>%
  chunk.data.source(n=10) 

Sal.Decadal <- c(Sal.Decadal,NorCPM.sal.src.i1,NorCPM.sal.src.i2)



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
                               sources=list("data_srcs/Observations/HadISST/HadISST_sst.nc"))
SST_obs$EN4  <- data.source(name="EN4",
                            type="Observations",
                            var="temperature",
                            sources=list(dir("data_srcs/Observations/EN4/",
                                             pattern="\\.zip$",full.names = TRUE,recursive=TRUE)))

Sal.obs <- list()
Sal.obs$EN4  <- data.source(name="EN4",
                            type="Observations",
                            var="salinity",
                            sources=list(dir("data_srcs/Observations/EN4/",
                                             pattern="\\.nc$",full.names = TRUE,recursive=TRUE)),
                            layermids.fn = function(f) {
                              #z.idx are the indices, v is the vertical coordinate in metres
                              ncid <- nc_open(f)
                              layer.mids <- ncid$dim$depth$vals
                              nc_close(ncid)
                              return(layer.mids)})


# ========================================================================
# Setup NMME models
# ========================================================================
library(readr)
NMME.cfg <- 
  read_csv2(file.path(PE.cfg$dirs$datasrc,"NMME","NMME_SST_urls.csv"),
                      col_types = cols()) %>%
  filter(active)
NMME.mdls <- split(NMME.cfg,NMME.cfg$Model)
NMME.sst.l <- list()
for(mdl.name in names(NMME.mdls)){
  mdl <- NMME.mdls[[mdl.name]]
  mdl.src <- mdl$URL
  names(mdl.src) <- mdl$type
  obj <- data.source(name=mdl.name,#sprintf("NMME-%s",mdl.name),
                     type="NMME",
                     var="sst",
                     sources=list(mdl.src))  
  NMME.sst.l[[mdl.name]] <- obj
}

#Restrict some realisations
#NMME.sst.l[["NASA-GEOS5"]]@realizations <- as.character(1:11)  #12th realization is very intermittant
#NMME.sst.l[["NCEP-CFSv2"]]@realizations <- as.character(1:24)  #Forecast has 32 but hindcast 24. 



#Import local CMIP5 metadata base
#Note that this is generated using F0... and can be
#legitimately copied from e.g. the HPC
CMIP5.db <- readRDS("objects/CMIP5db.rds")


#Function to make data.sources for CMIP5 slot
make.CMIP5.srcs <- function(meta,var) {
  #Select variable of interest
  stopifnot(length(var)==1)
  meta <- filter(meta,variable==var)
  
  #Setup vert selection function
  layermids.fn <- function(f) {
    #z.idx are the indices, v is the vertical coordinate in metres
    ncid <- nc_open(f)
    stopifnot(length(ncid$dim$lev)!=0)  #Must be a level variable
    stopifnot(ncid$dim$lev$units=="m")  #with units of metres
    layer.mids <- ncid$dim$lev$vals
    nc_close(ncid)
    return(layer.mids)}
  
  
  
  #Split the remaining CMIP5 data into individual sources
  mdl.l <- split(meta,meta$model)
  rtn <- lapply(mdl.l,function(f) {
    data.source(name=unique(f$model),
                type="CMIP5",
                layermids.fn = layermids.fn,
                var=var,
                sources=list(f$fname))  
  })
  return(rtn)
}
