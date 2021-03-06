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
  library(pbapply)
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
log_msg("Decadal SST models...\n")
SST.Decadal <- PElst()

#MPI-LR
#There is a problem with the date-time stamps in one of the files in this hindcast
#ensemble. It could be corrected e.g. by copying the dates from a neighbouring
#realisation, but to start with we just drop it
# MPI.LR.SST.srcs <- dir(here(PE.cfg$dir$datasrc,"Decadal","MPI-ESM-LR_MiKlip-b1","thetao"),
#                     pattern="\\.nc$",full.names = TRUE) %>%
#                  subset(!grepl("thetao_Omon_MPI-ESM-LR_decs4e1964_r3i1p1_196501-197412.nc",.))
# 
# SST.Decadal$"MPI-LR" <-  
#   data.source(name="MPI.ESM.LR",
#               type="Decadal",
#               var="thetao",
#               fields.are.2D = FALSE,
#               z2idx = function(z,f) {
#                 ncid <- nc_open(f)
#                 z.bnds <- ncvar_get(ncid,"lev_bnds")
#                 idxs <- bounds.to.indices(z,z.bnds[1,],z.bnds[2,])
#                 nc_close(ncid)
#                 return(idxs)},
#               sources=MPI.LR.SST.srcs,
#               realization.fn=CMIP5_realisation,
#               start.date=function(f){
#                 init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
#                 init.date <- ymd(paste(init.str,"01",sep=""))
#                 return(init.date)},
#               date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 

#New CMIP6 MPI HER runs
SST.Decadal$"MPI-ESM1-2-HER" <-
  data.source(name="MPI-ESM1-2-HER",
              type="Decadal",
              var="tos",
              fields.are.2D=TRUE,
              sources=dir(here(PE.cfg$dir$datasrc,"Decadal","MPI-ESM1-2-HER","tos"),
                          pattern="*.nc",full.names = TRUE),
              date.fn =function(f) {return(floor_date(cdo.dates(f),"month"))},
              start.date=function(f){
                syear <- gsub("^s([[:digit:]]{4})-r.*$",
                              "\\1",
                              underscore_field(basename(f),5))
                return(as.Date(ISOdate(as.numeric(syear)+1,1,1)))}, #Round  up to 1 Jan
              realization.fn = function(f) {
                gsub("^.*_s[[:digit:]]{4}-(r.*?)_.*$","\\1",basename(f))})

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

#HadGem3
HadGEM3.fnames <-
  dir(here(PE.cfg$dir$datasrc,"Decadal","HadGEM3-GC31-MM"),
      pattern="*.nc$",recursive=TRUE,full.names=TRUE) %>%
  CMIP6.filename.extr()

SST.Decadal$HadGEM3 <- 
  data.source(name="HadGEM3",
              type="Decadal",
              var="tos",
              fields.are.2D=TRUE,
              sources=filter(HadGEM3.fnames,
                             field=="tos",
                             table=="Omon",
                             model=="HadGEM3-GC31-MM",
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
log_msg("Decadal Salinity models...\n")
Sal.Decadal <- PElst()

#MPI-LR
#There is a problem with the date-time stamps on the first realisation in this hindcast
#ensemble. It could be corrected, but to start with we just drop it, awaiting the updated
#data set from Daniela. 2019.06.04
# MPI.LR.srcs <- dir(file.path(decadal.dir,"MPI-ESM-LR_MiKlip-b1","so"),
#                    pattern="\\.nc$",full.names = TRUE) %>%
#                 subset(!grepl("r1i1p1",.))
#Received the corrected files and the full 10 member ensemble, so this should work properly now
# MPI.LR.so.srcs <- dir(here(PE.cfg$dir$datasrc,"Decadal","MPI-ESM-LR_MiKlip-b1","so_10member"),
#                    pattern="\\.nc$",full.names = TRUE) 
# 
# Sal.Decadal$"MPI-LR" <-  
#   data.source(name="MPI.ESM.LR",
#               type="Decadal",
#               var="so",
#               fields.are.2D = FALSE,
#               z2idx = SST.Decadal$"MPI-LR"@z2idx,
#               sources=MPI.LR.so.srcs,
#               realization.fn=CMIP5_realisation,
#               start.date=function(f){
#                 init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
#                 init.date <- ymd(paste(init.str,"01",sep=""))
#                 return(init.date)},
#               date.fn=function(f) {return(floor_date(cdo.dates(f),"month"))}) 


#New CMIP6 MPI HER runs
Sal.Decadal$"MPI-ESM1-2-HER" <-
  new("data.source",SST.Decadal$"MPI-ESM1-2-HER",
      var="so",
      fields.are.2D=FALSE,
      sources=dir(here(PE.cfg$dir$datasrc,"Decadal","MPI-ESM1-2-HER","so"),
                  pattern="*.nc",full.names = TRUE),
      z2idx=function(z,f) {
        ncid <- nc_open(f)
        lev_bnds <- ncvar_get(ncid,"lev_bnds")
        nc_close(ncid)
        idxs <- bounds.to.indices(z,lev_bnds[1,],lev_bnds[2,])
        return(idxs)})
      

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
#HadGEM3
Sal.Decadal$HadGEM3 <- 
  d<-new("data.source",
         SST.Decadal$HadGEM3,
         var="so",
         fields.are.2D=FALSE,
         z2idx=function(z,f) {
           ncid <- nc_open(f)
           lev_bnds <- ncvar_get(ncid,"lev_bnds")
           nc_close(ncid)
           idxs <- bounds.to.indices(z,lev_bnds[1,],lev_bnds[2,])
           return(idxs)},
         sources=filter(HadGEM3.fnames,
                        field=="so",
                        table=="Omon",
                        model=="HadGEM3-GC31-MM",
                        experiment=="dcppA-hindcast",
                        grid=="gn")$path) 


#'========================================================================
# Sea Level Pressure ####
#'========================================================================
log_msg("Decadal SLP models...\n")
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
                val <- str_match(basename(f),
                                 "^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,3]
                return(val)},
              start.date=function(f) {
                val <- str_match(basename(f),
                                 "^b.e11.BDP.f09_g16.([0-9]{4}-[0-9]{2}).([0-9]{3}).*$")[,2]
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
log_msg("Observations...\n")
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
                                        pattern="\\.zip$",
                                        full.names = TRUE,recursive=TRUE))

Sal.obs <- PElst()
Sal.obs$EN4  <- data.source(name="EN4",
                            type="Observations",
                            var="salinity",
                            fields.are.2D = FALSE,
                            z2idx=SST_obs$EN4@z2idx,
                            date.fn=SST_obs$EN@date.fn,
                            sources=dir(here(PE.cfg$dir$datasrc, "Observations","EN4"),
                                        pattern="\\.nc$",
                                        full.names = TRUE,recursive=TRUE))

Sal.obs$ORAS4 <- data.source(name="ORAS4",
                             type="Observations",
                             var="so",
                             fields.are.2D=FALSE,
                             realization.fn = function(...) return(NA),
                             start.date = function(...) return(NA),
                             z2idx=function(z,f){
                               ncid <- nc_open(f)
                               depths <- ncvar_get(ncid,"depth")
                               nc_close(ncid)
                               idxs <- midpoints.to.indices(z,depths)
                               return(idxs)},
                             date.fn = cdo.dates,
                             sources=dir(here(PE.cfg$dir$datasrc,"Observations","ORAS4"),
                                         pattern="^so_.*nc$",
                                         full.names = TRUE,recursive=TRUE))

Sal.obs$ORAS5  <- data.source(name="ORAS5",
                             type="Observations",
                             var="vosaline",
                             fields.are.2D=FALSE,
                             realization.fn = function(f)  {
                               return(basename(dirname(f)))},
                             start.date = function(...) return(NA),
                             z2idx=function(z,f){
                               ncid <- nc_open(f)
                               depths <- ncvar_get(ncid,"deptht")
                               nc_close(ncid)
                               idxs <- midpoints.to.indices(z,depths)
                               return(idxs)},
                             date.fn = cdo.dates,
                             sources=dir(here(PE.cfg$dir$datasrc,"Observations","ORAS5"),
                                         pattern="^vosaline_.*nc$",
                                         full.names = TRUE,
                                         recursive=TRUE))



#Sea level pressure
SLP.obs <- PElst()
SLP.obs$HadSLP2 <- data.source(name="HadSLP2",
                               type="Observations",
                               var="SLP",
                               fields.are.2D = TRUE)

#'========================================================================
# NMME ####
#'========================================================================
log_msg("NMME...\n")
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
log_msg("CMIP6...\n")
#Tweak datasources
CMIP6.db.all <- 
  #Load metadata
  readRDS(file=PE.cfg$path$CMIP.metadata)

#Remove
# * Only one type of grid - gn
# * Models on unstructured grids
grid.ranking <- c("gn","gr","gr1")
assert_that(all(CMIP6.db.all$grid %in% grid.ranking),
            msg="Unknown grid type found")
assert_that(all(CMIP6.db.all$table=="Omon"),
                msg="Non-monthly data has snuck in")

CMIP6.db.grid.sel <- 
  CMIP6.db.all %>%
  #Remove unstructured grids, as CDO (remapbil etc) can't interpolate on these
  #We define an unstructured grid based on the number of cells - anything greater
  #than 10000 looks to be the cutoff at the moment
  filter(max.xy.dim < 10000) %>% 
  #Remove irregular vertical layers from salinity variable
  #Particular, models on density layers
  filter(!(variable=="so" & zaxis.name %in% c("sea_water_potential_density","ocean_sigma_z"))) %>% 
  #Drop vertical axes that are not in metres. This could be handled, but its more robust to leave it out
  filter(!(variable=="so" & zaxis.units=="centimeters")) %>% 
  #Choose grid preferred grid
  mutate(grid.pref=as.numeric(factor(grid,grid.ranking)))  %>%
  group_by(variable,table,source) %>%
  filter(grid.pref==min(grid.pref)) %>%  #Take the most preferrred option
  ungroup() 


#Only include models that we have the full coverage of the relevant time period
#TODO: Could also check this against what is in the files
time.range <- seq(ymd("1960-01-01"),ymd("2029-12-01"),by="month")
CMIP6.db <-
  #Split out time range and get months that (should be) in the files
  CMIP6.db.grid.sel %>% 
  separate(time_range,c("time_start","time_end"),sep="-") %>% 
  mutate(time_start=ymd(paste0(time_start,"01")),
         time_end=ymd(paste0(time_end,"01")),
         months=map2(time_start,time_end, ~ seq(.x,.y,by="months"))) %>% 
  #Group by souce and variable, and check for full coverage
  nest(data=-c(variable,table,source)) %>%
  hoist(data,months="months") %>%
  mutate(months=map(months,~do.call(c,.x)),
         time.range.ok=map_lgl(months,~all(time.range %in% .x))) %>% 
  filter(time.range.ok) %>%
  #Tidy up
  select(-months,time.range.ok) %>% 
  unnest(data)

#Setup the CMIP6 template object
#Note that here we define the name of the model as the realization - this is ok
#when we are using one realizations per model, but obviously breaks down when we are
#starting to use multiple realisations. We would therefore need to have two
#different types of objects, one for dealing with multiple realisations and a
#streamlined version otherwise. Also need to account for
#different SSPs as well, at some point. But for now, one realisation.
CMIP6.template <- 
  data.source(type="CMIP6",
              name="CMIP6.uninit",
              z2idx=function(z,f) {
                lvls.txt <- cdo("showlevel",f)
                lvls.num <- scan(text=str_trim(lvls.txt),sep=" ",quiet=TRUE)
                assert_that(!any(is.na(lvls.num)),
                            msg=sprintf("Error in extracting levels from %s",f))
                #Extract indices
                idxs <- which(lvls.num>=min(z) & lvls.num <= max(z))
                assert_that(!any(is.na(idxs)),
                            msg=sprintf("Error in extracting indicies from %s",f))
                return(idxs)},
              realization.fn = function(f) {
                sprintf("%s/%s",underscore_field(f,3),underscore_field(f,5))},  #Use model name for realization
              start.date=function(f){NA}, #No start date
              date.fn=function(f) {return(floor_date(ncdump.times(f),"month"))}) 

CMIP6.datasrcs <-
  #Nest to make things easier to work with
  CMIP6.db %>%
  nest(fnames=-c(variable)) %>%
  #Create objects
  mutate(fields.are.2D=case_when(variable =="tos"~TRUE,
                                 variable == "so" ~FALSE,
                                 TRUE ~ NA)) %>%
  mutate(data.srcs=pmap(.,function(...) {
    d <- list(...)
    new("data.source",CMIP6.template,
        fields.are.2D=d$fields.are.2D,
        var=d$variable,
        sources=d$fnames$path)})) %>%
  pull(data.srcs) %>% 
  PElst()

#'========================================================================
# Finish ####
#'========================================================================
log_msg("Finalising...\n")
#Combine into a tibble for storage and selection
src.list <- 
  list("SST.obs"=SST_obs,
       "SST.Decadal"=SST.Decadal,
       "SST.NMME"=NMME.sst,
       "Sal.obs"=Sal.obs,
       "Sal.Decadal"=Sal.Decadal,
       "SLP.Decadal"=SLP.Decadal,
       "SLP.obs"=SLP.obs,
       "CMIP6.uninit"=CMIP6.datasrcs)

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

