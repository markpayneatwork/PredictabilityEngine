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
# Initialise system
#'========================================================================
cat(sprintf("\n%s\n","Common Data Sources Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
})

#'========================================================================
# Helper functions ####
#'========================================================================
CMIP6.filename.extr <- function(p) {
  tibble(path=p,
         fname=basename(p)) %>%
  separate(fname,
           into=c("field","table","model","experiment","variant","grid","time"),
           sep="_") %>%
    separate(variant,
             into=c("start","realization"),
             sep="-") %>%
    extract(realization,
            c("realization","initialization","other"),
            "^(r[[:digit:]]+)(i[[:digit:]]+)(.+)$")
}

#'========================================================================
# SST.Decadal models ####
#'========================================================================
#IPSL and MPI-MR have basically an identifical structure, both being produced
#originally by SPECS
SST.Decadal <- PElst()
# SST.Decadal$IPSL  <- data.source(name="IPSL-CM5A-LR",
#                                  type="Decadal",
#                                  sources=list(dir(file.path(decadal.dir,"IPSL-CM5A-LR"),
#                                                   pattern="\\.nc$",full.names = TRUE)),
#                                  var="tos",
#                                  realization.fn=function(f) {
#                                    underscore_field(f,6)},
#                                  start.date=function(f){
#                                    init.str <- gsub("S","",underscore_field(f,5))
#                                    init.date <- ymd(init.str)
#                                    return(init.date)},
#                                  date.fn=function(f,varname="tos") {
#                                      if(length(f)>1) stop("Function not vectorised")
#                                      dates <- getZ(brick(f,varname=varname))
#                                      return(floor_date(dates,"month"))})
# 
# SST.Decadal$"MPI-MR" <-  new("data.source",
#                              SST.Decadal$IPSL,
#                              name="MPI-ESM-MR",
#                              sources=list(dir(file.path(decadal.dir,"MPI-ESM-MR"),
#                                               pattern="\\.nc$",full.names = TRUE)))

#MPI-LR
#There is a problem with the date-time stamps in one of the files in this hindcast
#ensemble. It could be corrected e.g. by copying the dates from a neighbouring
#realisation, but to start with we just drop it
MPI.LR.SST.srcs <- dir(here(PE.cfg$dir$datasrc,"Decadal","MPI-ESM-LR_MiKlip-b1","thetao"),
                    pattern="\\.nc$",full.names = TRUE) %>%
                 subset(!grepl("thetao_Omon_MPI-ESM-LR_decs4e1964_r3i1p1_196501-197412.nc",.))

SST.Decadal$"MPI-LR" <-  
  data.source(name="MPI.ESM.LR",
              type="Decadal",
              var="thetao",
              fields.are.2D = FALSE,
              z2idx = function(z,f) {
                ncid <- nc_open(f)
                z.bnds <- ncvar_get(ncid,"lev_bnds")
                idxs <- bounds.to.indices(z,z.bnds[1,],z.bnds[2,])
                nc_close(ncid)
                return(idxs)},
              sources=MPI.LR.SST.srcs,
              realization.fn=CMIP5_realisation,
              start.date=function(f){
                init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                init.date <- ymd(paste(init.str,"01",sep=""))
                return(init.date)},
              date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 

#MPI - NCEP requires all three to be specified
# SST.Decadal$"MPI-NCEP" <- data.source(name="MPI-NCEP-forced",
#                                       type="Decadal",
#                                       sources=list(dir(file.path(decadal.dir,"MPI-ESM-LR_NCEP-forced"),
#                                                        pattern="\\.nc$",full.names = TRUE)),
#                                       var="var2",
#                                       realization.fn=function(f){return(rep("r1",length(f)))},
#                                       date.fn=function(f){
#                                         stop("Needs to be checked")
#                                         dates.str <- getZ(brick(f))
#                                         dates <- ymd(dates.str)
#                                         day(dates) <- 15  #Adjust to be mid month, rather than end of month
#                                         #But adjust
#                                         return(dates)},
#                                       start.date=function(f){
#                                         init.yr <- str_match(basename(f),"^dma([0-9]{4})_.*$")[,2]
#                                         init.date <- as.Date(ISOdate(init.yr,1,1))
#                                         return(init.date)})

#GFDL is largely in CMIP5 format
# SST.Decadal$GFDL <-   data.source(name="GFDL-CM2.1",
#                                   type="Decadal",
#                                   sources=list(dir(file.path(decadal.dir,"GFDL-CM2.1"),
#                                                    pattern="\\.nc$",full.names = TRUE)),
#                                   var="tos",
#                                   realization.fn=CMIP5_realisation,
#                                   start.date=function(f){
#                                     init.yr <- str_match(basename(f),"^.*?_decadal([0-9]{4})_r.*$")[,2]
#                                     init.date <- as.Date(ISOdate(init.yr,1,1))
#                                     return(init.date)},
#                                   date.fn=date.by.brick)

#Add in the CESM DPLE
SST.Decadal$CESM.DPLE <-   
  data.source(name="CESM.DPLE",
              var="SST",
              type="Decadal",
              fields.are.2D = TRUE,
              sources=dir(here(PE.cfg$dir$datasrc,"Decadal","CESM-DPLE","SST"),
                          pattern="\\.nc$",full.names = TRUE),
              use.timebounds=3,
              realization.fn=function(f) {
                val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,3]
                return(val)},
              start.date=function(f) {
                val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,2]
                init.date <- ymd(sprintf("%s-01",val))
                return(ceiling_date(init.date,"year"))},  #Round November start up to 1 Jan
              date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 

#NorCPM
#Note that there are two initialisations here, stored in the same directory - i1 and i2.
#We treat them as different data sources for the purpose of this analysis
NorCPM.fnames <- 
  dir(here(PE.cfg$dir$datasrc,"Decadal","NorCPM"),
      pattern="*.nc$",recursive = TRUE,full.names = TRUE) %>%
  CMIP6.filename.extr()

SST.Decadal$NorCPM.SST.src <- 
  data.source(name="NorCPM",
              var="tos",
              type="Decadal",
              fields.are.2D=TRUE,
              sources=filter(NorCPM.fnames,
                             field=="tos")$path,
              realization.fn = function(f) {
                gsub("^.*_s[[:digit:]]{4}-(r.*?)_.*$","\\1",basename(f))},
              start.date=function(f){
                syear <- gsub("^s([[:digit:]]{4})-r.*$",
                              "\\1",
                              underscore_field(basename(f),5))
                return(as.Date(ISOdate(as.numeric(syear)+1,1,1)))}, #Round  up to 1 Jan
              date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 

# SST.Decadal$NorCPM.SST.src.i1 <- 
#   new("data.source",
#       SST.Decadal$NorCPM.SST.src,
#       name="NorCPM.i1",
#       sources=filter(NorCPM.fnames,
#                           field=="tos",
#                           initialization=="i1")$path)
# 
# SST.Decadal$NorCPM.SST.src.i2 <- 
#   new("data.source",
#       SST.Decadal$NorCPM.SST.src,
#       name="NorCPM.i2",
#       sources=filter(NorCPM.fnames,
#                      field=="tos",
#                      initialization=="i2")$path)

#EC-Earth3
ECEarth.fnames <-
  dir(here(PE.cfg$dir$datasrc,"Decadal","EC-Earth3"),
      pattern="*.nc$",recursive=TRUE,full.names=TRUE) %>%
  CMIP6.filename.extr()

SST.Decadal$ECEarth3 <- 
  data.source(name="ECEarth3",
              type="Decadal",
              var="tos",
              fields.are.2D=TRUE,
              sources=filter(ECEarth.fnames,
                             field=="tos",
                             table=="Omon",
                             model=="EC-Earth3",
                             experiment=="dcppA-hindcast",
                             grid=="gn")$path,
              realization.fn = function(f) {
                gsub("^.*_s[[:digit:]]{4}-(r.*?)_.*$","\\1",basename(f))},
              start.date=function(f){
                syear <- gsub("^s([[:digit:]]{4})-r.*$",
                              "\\1",
                              underscore_field(basename(f),5))
                return(as.Date(ISOdate(as.numeric(syear)+1,1,1)))}, #Round  up to 1 Jan
              date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 


#'========================================================================
# Salinity  ####
#'========================================================================
Sal.Decadal <- PElst()

#MPI-LR
#There is a problem with the date-time stamps on the first realisation in this hindcast
#ensemble. It could be corrected, but to start with we just drop it, awaiting the updated
#data set from Daniela. 2019.06.04
# MPI.LR.srcs <- dir(file.path(decadal.dir,"MPI-ESM-LR_MiKlip-b1","so"),
#                    pattern="\\.nc$",full.names = TRUE) %>%
#                 subset(!grepl("r1i1p1",.))
#Received the corrected files and the full 10 member ensemble, so this should work properly now
MPI.LR.so.srcs <- dir(here(PE.cfg$dir$datasrc,"Decadal","MPI-ESM-LR_MiKlip-b1","so_10member"),
                   pattern="\\.nc$",full.names = TRUE) 

Sal.Decadal$"MPI-LR" <-  
  data.source(name="MPI.ESM.LR",
              type="Decadal",
              var="so",
              fields.are.2D = FALSE,
              z2idx = SST.Decadal$"MPI-LR"@z2idx,
              sources=MPI.LR.so.srcs,
              realization.fn=CMIP5_realisation,
              start.date=function(f){
                init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                init.date <- ymd(paste(init.str,"01",sep=""))
                return(init.date)},
              date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 

#CESM-DPLE
Sal.Decadal$CESM.DPLE.SALT <- 
  data.source(SST.Decadal$CESM.DPLE,
              var="SALT",
              sources=dir(here(PE.cfg$dir$datasrc,"Decadal","CESM-DPLE","SALT"),
                          pattern="\\.nc$",full.names = TRUE),
              fields.are.2D = FALSE,
              z2idx = function(z,f) {
                ncid <- nc_open(f)
                #The original z_w_top and z_w_bot are dropped by cdo when
                #preprocessing, so need to base it on z_w and dz instead, which
                #are retained.
                # z_w_top <- ncvar_get(ncid,"z_w_top")/100 #convert cm -> m
                # z_w_bot <- ncvar_get(ncid,"z_w_bot")/100 #convert cm -> m
                z_w_top <- ncvar_get(ncid,"z_w")/100
                dz <- ncvar_get(ncid,"dz")/100
                z_w_bot <- z_w_top +dz
                nc_close(ncid)
                idxs <- bounds.to.indices(z,z_w_top,z_w_bot)
                return(idxs)},
              date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 

Sal.Decadal$NorCPM.sal.src <- 
  new("data.source",
      var="so",
      SST.Decadal$NorCPM.SST.src,
      name="NorCPM",
      fields.are.2D=FALSE,
      z2idx=function(z,f) {
        ncid <- nc_open(f)
        lev_bnds <- ncvar_get(ncid,"lev_bnds")
        nc_close(ncid)
        idxs <- bounds.to.indices(z,lev_bnds[1,],lev_bnds[2,])
        return(idxs)},
      sources=filter(NorCPM.fnames,
                     field=="so",
                     grid=="gr")$path)

# Sal.Decadal$NorCPM.sal.src.i1 <- 
#   new("data.source",
#       Sal.Decadal$NorCPM.sal.src,
#       name="NorCPM.i1",
#       sources=filter(NorCPM.fnames,
#                      field=="so",
#                      grid=="gr",
#                      initialization=="i1")$path)
# 
# Sal.Decadal$NorCPM.sal.src.i2 <- 
#   new("data.source",
#       Sal.Decadal$NorCPM.sal.src,
#       name="NorCPM.i2",
#       sources=filter(NorCPM.fnames,
#                           field=="so",
#                           grid=="gr",
#                           initialization=="i2")$path)

#EC-Earth3
ECEarth.fnames <-
  dir(here(PE.cfg$dir$datasrc,"Decadal","EC-Earth3"),
      pattern="*.nc$",recursive=TRUE,full.names=TRUE) %>%
  CMIP6.filename.extr()

Sal.Decadal$ECEarth3 <- 
  d<-new("data.source",
      SST.Decadal$ECEarth3,
      var="so",
      fields.are.2D=FALSE,
      z2idx=function(z,f) {
        ncid <- nc_open(f)
        lev_bnds <- ncvar_get(ncid,"lev_bnds")
        nc_close(ncid)
        idxs <- bounds.to.indices(z,lev_bnds[1,],lev_bnds[2,])
        return(idxs)},
      sources=filter(ECEarth.fnames,
                     field=="so",
                     table=="Omon",
                     model=="EC-Earth3",
                     experiment=="dcppA-hindcast",
                     grid=="gn")$path) 


#'========================================================================
# Sea Level Pressure ####
#'========================================================================
SLP.Decadal <- PElst()

#Add in the CESM DPLE
SLP.Decadal$CESM.DPLE <-   
  data.source(name="CESM.DPLE",
              var="PSL",
              type="Decadal",
              fields.are.2D = TRUE,
              sources=dir(here(PE.cfg$dir$datasrc,"Decadal","CESM-DPLE","PSL"),
                          pattern="\\.nc$",full.names = TRUE),
              use.timebounds=3,
              realization.fn=function(f) {
                val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,3]
                return(val)},
              start.date=function(f) {
                val <- str_match(basename(f),"^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,2]
                init.date <- ymd(sprintf("%s-01",val))
                return(ceiling_date(init.date,"year"))},  #Round November start up to 1 Jan
              date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 

#'========================================================================
# Uninitialised models ####
#'========================================================================
# These should largely be the same...
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

#'========================================================================
# Observations ####
#'========================================================================
SST_obs <- PElst()
SST_obs$HadISST <- data.source(name="HadISST",
                               type="Observations",
                               var="sst",
                               fields.are.2D = TRUE,
                               sources="data_srcs/Observations/HadISST/HadISST_sst.nc")
SST_obs$EN4  <- data.source(name="EN4",
                            type="Observations",
                            var="temperature",
                            fields.are.2D = FALSE,
                            z2idx=function(z,f){
                              ncid <- nc_open(f)
                              depth_bnds <- ncvar_get(ncid,"depth_bnds")
                              nc_close(ncid)
                              idxs <- bounds.to.indices(z,depth_bnds[1,],depth_bnds[2,])
                              return(idxs)},
                            date.fn = function(f) {
                              ncid <- nc_open(f)
                              time.var <- ncvar_get(ncid,"time")
                              nc_close(ncid)
                              return(time.var+ as.Date("1800-01-01"))
                            },
                            sources=dir(here(PE.cfg$dir$datasrc,"Observations","EN4"),
                                             pattern="\\.zip$",full.names = TRUE,recursive=TRUE))

Sal.obs <- PElst()
Sal.obs$EN4  <- data.source(name="EN4",
                            type="Observations",
                            var="salinity",
                            fields.are.2D = FALSE,
                            z2idx=SST_obs$EN4@z2idx,
                            date.fn=SST_obs$EN@date.fn,
                            sources=dir(here(PE.cfg$dir$datasrc, "Observations","EN4"),
                                             pattern="\\.nc$",full.names = TRUE,recursive=TRUE))

#Sea level pressure
SLP.obs <- PElst()
SLP.obs$HadSLP2 <- data.source(name="HadSLP2",
                               type="Observations",
                               var="SLP",
                               fields.are.2D = TRUE)

#'========================================================================
# NMME ####
#'========================================================================
library(readr)
NMME.cfg <- 
  read_csv2(here(PE.cfg$dir$datasrc,"NMME","NMME_SST_urls.csv"),
                      col_types = cols()) %>%
  filter(active)
NMME.mdls <- split(NMME.cfg,NMME.cfg$Model)
NMME.sst <- PElst()
for(mdl.name in names(NMME.mdls)){
  mdl <- NMME.mdls[[mdl.name]]
  mdl.src <- mdl$URL
  names(mdl.src) <- mdl$type
  obj <- data.source(name=mdl.name,#sprintf("NMME-%s",mdl.name),
                     type="NMME",
                     var="sst",
                     fields.are.2D = TRUE,
                     sources=mdl.src)  
  NMME.sst[[mdl.name]] <- obj
}

#Restrict some realisations
#NMME.sst.l[["NASA-GEOS5"]]@realizations <- as.character(1:11)  #12th realization is very intermittant
#NMME.sst.l[["NCEP-CFSv2"]]@realizations <- as.character(1:24)  #Forecast has 32 but hindcast 24. 

#'========================================================================
# CMIP6 ####
#'========================================================================
#Setup CMIP6 database of filenames first
#File naming convention, from https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit
#<variable_id>_<table_id>_<source_id>_<experiment_id >_<member_id>_<grid_label>[_<time_range>].nc
CMIP6.db.all <- 
  tibble(path=dir(here(PE.cfg$dir$datasrc,"CMIP6"),pattern="*.nc",full.names = TRUE)) %>%
  mutate(file.size=file.size(path),
         fname=basename(path)) %>%
  separate(fname,sep="_",
           into=c("variable","table","source","experiment","member","grid","time_range"))

#Print some summary data
CMIP6.db.all %>%
  filter(file.size!=0) %>%
  count(variable,source,grid) %>%
  pivot_wider(names_from=c(grid),values_from=n) %>%
  print(n=Inf)

#Setup the CMIP6 template object
CMIP6.template <- 
  data.source(type="CMIP6",
              name="historical",
              realization.fn = function(f) {
                gsub("^.*?_.*?_(.*?)_.*$","\\1",basename(f))},
              start.date=function(f){NA}, #No start date
              date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 

#Remove
# * Partial files
# * Only one type of grid - gn
# * Only historical experiments
CMIP6.db <- 
  CMIP6.db.all %>%
  filter(file.size!=0) %>%
  filter(experiment=="historical",
         grid=="gn") %>%
  nest(data=-c(variable)) %>%
  #Create objects
  mutate(fields.are.2D=case_when(variable =="tos"~TRUE,
                                 variable == "so" ~FALSE,
                                 TRUE ~ NA)) %>%
  mutate(data.srcs=pmap(.,function(...) {
    d <- list(...)
    new("data.source",CMIP6.template,
        fields.are.2D=d$fields.are.2D,
        var=d$variable,
        sources=d$data$path)})) %>%
  pull(data.srcs) %>% 
  PElst()

#'========================================================================
# Finish ####
#'========================================================================
#Combine into a tibble for storage and selection
src.list <- 
  list("SST.obs"=SST_obs,
       "SST.Decadal"=SST.Decadal,
       "SST.NMME"=NMME.sst,
       "Sal.obs"=Sal.obs,
       "Sal.Decadal"=Sal.Decadal,
       "SLP.Decadal"=SLP.Decadal,
       "SLP.obs"=SLP.obs,
       "CMIP6"=CMIP6.db)

src.tb <- 
  enframe(src.list,name="group",value = "sources") %>%
  mutate(sources=map(sources,as.list)) %>%
  unnest(sources) %>%
  mutate(srcType=map_chr(sources,slot,"type"),
         srcName=map_chr(sources,slot,"name"),
         var=map_chr(sources,slot,"var"),
         n.srcs=map_dbl(sources,~ length(slot(.x,"sources")))) %>%
  arrange(group,srcType,srcName)

these.srcs <-
  src.tb %>%
  filter(n.srcs!=0)

saveRDS(these.srcs,file=PE.cfg$path$datasrcs)

#Turn off thte lights
log_msg("\nConfiguration complete.\n")

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

