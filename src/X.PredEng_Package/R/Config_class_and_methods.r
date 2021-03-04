setClassUnion("data.sourceORNULL",c("data.source","NULL"))


#' PredEng Project configuration class
#'
#' @slot project.name Name of the configuration
#' @slot Observations A data.source object defining the observational dataset to include. 
#' @slot Decadal A list of GCM objects defining the decadal forecast systems to be analysed
#' @slot NMME A list of GCM objects defining the NMME models to be analysed
#' @slot CMIP5 A list of data source objects to configure extraction from the CMIP5 ensemble. 
#' @slot obs.only Only process observations. Useful for cases where we don't want to process everything.
#' @slot persistence.leads A vector (in months) of lead times at which to generate persistence forcasts
#' @slot spatial.polygons A sf data.frame defining the spatial domains over which  to operate
#' @slot statistics PredEng.list of statistics to apply over each spatial area
#' @slot pt.extraction A tibble defining the points to extract, with three columns 1) table - the database table to do 
#' the extraction from 2) filter - a character string defining a dplyr::filter command to subset the set of fields
#' in the table - extraction only tables place from these 3) points - a st_sf data frame defining the points at which
#' to extract and the date time ("date" column). Other metadata is carried forward into the output database
#' @slot calibrationMethods A vector listing the calibration methods to use. The full list of supported values is 
#' stored in PE.cfg$validity$calibrationMethods
#' @slot MOI The months of interest (a vector of integers between 1 and 12 inclusive). Currently only support one month
#' but this can be extened in the future 
#' @slot average.months Should we average over the months of interest? Currently redundant as we do not support multiple
#' month extraction, but may come back in the future
#' @slot vert.range The vertical range, in m, over which to average. NA indicates no vertical averaging (for 
#' 2D fields) or to use the surface layer (for 3D fields). 
#' @slot clim.years The years to include in the climatology and analysis of hindcast skill (vector of 
#' integers)
#' @slot comp.years The years over which to make comparisons between observations and models
#' @slot landmask The file to use as the basis for masking out land [optional]. If null, all pixels are used.
#' @slot scratch.dir Directory in which processed files are to be stored
#' @slot global.ROI  Extent object defining the global ROI to work with. 
#' @slot global.res Resolution of the analysis to be applied globally
#' @slot retain.realizations Is there interest in retaining the individual realisations from each model, or
#' should we go straight to the realisation means (as a way of saving disk space and simiplifying the
#' processing)?
#' @slot package.version Details of the package version that this object was built with. Written automatically by the set.configuration() function
#' @slot config.date Date/time when the configuration was finalised
#'
#' @include Data_source_class_and_methods.r
#' @include PElst.r
#' 
#' @details Extraction from the data sets can be performed either at specific geographical locations
#'  (for all  time points), at specific times (for all locations), or at specific points in space-time. 
#'  This extraction process is controlled by the extraction slot, which consists of a list of vectors, 
#'  as follows
#' \itemize{
#'   \item  A vector of dates on which to extract rasters e.g. to make spatial forecast maps
#'   \item An SpatialPointsDataFrame object from the sp package, indicating the points in
#' space and time that should be extracted
#' } 
#' Different extraction modes can be combined in the same list.
#' 
#' @export PredEng.config
PredEng.config <- 
  setClass("PredEng.config",
           slots=list(project.name="character",
                      Observations="data.sourceORNULL",
                      Decadal="PElst",
                      NMME="PElst",
                      CMIP5="PElst",
                      obs.only="logical",
                      persistence.leads="numeric",
                      spatial.polygons="sf",
                      statistics="PElst",
                      pt.extraction="data.frame",
                      calibrationMethods="character",
                      global.ROI="Extent",
                      global.res="numeric",
                      MOI="numeric",
                      vert.range="numeric",
                      clim.years="numeric",
                      comp.years="numeric",
                      landmask="character",
                      scratch.dir="character",
                      retain.realizations="logical",
                      average.months="logical",
                      config.date="character",
                      package.version="character"),
           prototype = list(global.ROI=extent(as.numeric(rep(NA,4))),
                            obs.only=FALSE,
                            persistence.leads=0:120,  #1-10 years
                            retain.realizations=TRUE,
                            vert.range=as.numeric(NA),  #Use surface unless specified
                            spatial.polygons=st_sf(st_sfc())),
           validity = function(object) {
             #Basics
             err.msg <- list(
               validate_that(!(length(object@MOI)!=1 & object@average.months),
                             msg="Dates are currently not handled correctly when averaging over multiple months"),
               validate_that(length(object@vert.range)==2 | all(is.na(object@vert.range)),
                             msg="Vertical range slot must be of length 2 if not NA"),
               validate_that(length(object@MOI)==1 | identical(object@calibrationMethods,"anomaly"),
                             msg="Multiple months of interest are currently not supported for calibrationMethods other than anomaly"),
               validate_that(all(object@MOI %in% 1:12),msg="Month(s) of interest must be in range 1-12"),
               validate_that(length(object@calibrationMethods)>0,msg="No calibration method defined"),
               validate_that(all(object@calibrationMethods %in% PE.cfg$validity$calibrationMethod),
                             msg="Unsupported calibration method selected"))
             
             #Check for consistency between presence of 2D fields and requested vertical range
             datsrcs <- c(object@Decadal,object@NMME,object@Observations,object@CMIP5)
             if(!all(is.na(object@vert.range))) {
               for(d in datsrcs) {
                 if(length(d@fields.are.2D)!=0) {
                   err.msg <- 
                     c(err.msg,
                       validate_that(!d@fields.are.2D,
                                     msg="When vertical range is specified, all data sources must be 3D"))
                 }
               }}
             
             #Check for consistency between spatial.polygons requested in the stats, and those available
             stat.spNames <- 
               map(object@statistics,slot,"spatial.polygons") %>% 
               unlist() %>%
               na.omit()
             err.msg$spName <-
               validate_that(length(stat.spNames)==0 | all(stat.spNames %in% object@spatial.polygons$name),
                             msg="Unknown spatial polygons requested by one or more statistics.")
             #
             err.idxs <- map_lgl(err.msg,is.character)
             if(all(!err.idxs)) return(TRUE) else unlist(err.msg[err.idxs])
           })



#' Visualise project region of interest
#' @export
setMethod("plot",signature(x="PredEng.config",y="missing"),
          function(x,y,...) {
            plt.sf <- 
              PE.global.sf(x) %>%
              mutate(name=sprintf("@%s",name))
            if(nrow(x@spatial.polygons)!=0) {
              plt.sf <- 
                x@spatial.polygons %>%
                dplyr::select(name,geometry) %>%
                rbind(plt.sf)
            }

            ggplot(data=plt.sf)+
              annotation_map(map_data("world"),fill="black",col="black")+
              geom_sf(aes(col=name,fill=name),alpha=0.25)+
              theme_bw()
          })


#' @export
setMethod("show","PredEng.config", function(object) {
  hdr.str <- paste( object@project.name,"configuration")
  cat(hdr.str,"\n")
  cat(paste(rep("-",nchar(hdr.str)),collapse="",sep=""),"\n")
  show.slot <- function(ob,slt) {
    obj <- slot(ob,slt)
    if(any(class(obj) %in% c("logical","formula","character",
                         "numeric","Extent","integer","list",
                         "data.frame","PElst","data.source"))) {
      cat(sprintf("%-20s : ",slt))
    } else {return(NULL)}
    if(is(obj,"formula")) {
      cat(deparse(obj,width.cutoff=options()$width),"\n")
    } else if(is(obj,"Extent")){
      cat(paste(obj[],collapse=", "),"\n")
    } else if(is(obj,"numeric") & length(obj) ==0){
      cat("\n")
    } else if(is(obj,"numeric") & length(obj) ==1){
      cat(obj,"\n")
    } else if(is(obj,"numeric") & length(obj) >=12){
      cat(paste(range(obj),collapse="-"),"\n")
    } else if(is(obj,"numeric") & length(obj) <12){
      cat(paste(range(obj),collapse="-"),"\n")
    } else if(is(obj,"data.frame")) {
      cat(sprintf("%s items \n",nrow(obj)))
    } else if(is(obj,"list")){
        #Get list names
        l.list <- length(obj)
        l.names <- names(obj)
        #Respond accordingly
        if(l.list != length(l.names) | l.list > 5) {
          cat(sprintf("%s items",length(obj)),"\n")
        } else if(length(l.names!=0)){
          cat(paste(l.names,collapse=", "),"\n")
        } else {
          cat("\n")
        }
          
   } else if(is(obj,"data.source") ){
      cat(obj@name,"\n")
    # } else if(is(obj,"Date") & length(obj) < 5){
    #   cat(as.character(obj),"\n")
    # } else if(is(obj,"Date")){
    #   cat(paste(length(obj),"Dates"),"\n")
    # } else if(is(obj,"SpatialPointsDataFrame") ){
    #   cat(paste(nrow(obj),"points"),"\n")
    } else {
      cat(paste(obj,collapse=", "),"\n")
    }
  }
  for(i in setdiff(slotNames("PredEng.config"),c("package.version","config.date"))) {
    show.slot(object,i)}
  cat(paste(rep("-",nchar(hdr.str)),collapse="",sep=""),"\n")
  log_msg("%-20s : %s\n","Configured on",object@config.date)
  log_msg("%-20s :\n","Git configuration")
  log_msg("%s\n",object@package.version)
})

#' #' Get vertical layers
#' #' 
#' #' Extracts a list of vertical layers
#' #'
#' #' @param cfg A PredEng configuration object, with the vert.range slot populated
#' #' @param src A data source object, containing a valid layermids.fn()
#' #' @param f The name of the file from which to extract the layers
#' #'
#' #' @return A vector of layer integers corresponding to the vertical range. In cases where the vertical
#' #' range limits fall within a layer, the layer is included if it covers more than 50%.
#' #' @export
#' #'
#' setGeneric("verticalLayers",function(cfg,src,...) standardGeneric("verticalLayers"))
#' 
#' setMethod("verticalLayers",signature = c("PredEng.config","data.source"),function(cfg,src,f){
#'   layer.mids <- src@layermids.fn(f)
#'   #Which layer are the vert.range boundaries in?
#'   layer.idxs <- round(approx(layer.mids,seq_along(layer.mids)+0.5,cfg@vert.range,method="linear",rule=2)$y)
#'   #Now return the appropriate value - if more than 50% of a layer is in, then include it
#'   return(min(layer.idxs):(max(layer.idxs)-1))})
#' 

#' Set the configuration
#'
#' @param object  A PredEng.config object
#'
#' @return The supplied object, with tweaks.
#' @export
set.configuration <- function(object) {
  #Check object is initially valid
  validObject(object)

  #Set output directories
  define_dir(object@scratch.dir)
  
  #Storage package version
  object@config.date <- date()
  object@package.version <- PE.current.version()

  #Write CDO grid descriptors
  griddes.txt <- griddes(object@global.ROI,res=object@global.res)
  writeLines(griddes.txt,PE.scratch.path(object,"analysis.grid"))

  #Output configuration file
  cfg.fname <- PE.scratch.path(object,"config")
  #cfg.linked <- PE.cfg$path$config
  saveRDS(object,file=cfg.fname)

  #Setup targets cache directory
  targets.link <- here("_targets")
  if(file.exists(targets.link)) {
    file.remove(targets.link)
  }
  scratch.tar <- define_dir(here(object@scratch.dir,"_targets"))
  define_dir(scratch.tar,"logs")
  file.symlink(scratch.tar,targets.link)
  
  #Final check
  validObject(object,complete=TRUE)
  show(object)

  return(object)
}

#' Import PredEng configuration
#' 
#' Loads the currently configured PredEng.config object. Configuration is defined according to the
#' configuration file linked into the project directory.
#'
#' @return PredEng.config object
#' @export
PE.load.config <- function() {
  #Find configuration file
  rfnames <- dir(here(),pattern="*.r$",full.names = TRUE)
  assert_that(length(rfnames)==1,msg="More than one configuration file found.")
  #Load corresponding configuration from scratch
  cfg.fname  <- 
    rfnames %>%
    gsub(".r$","",.) %>%
    basename() %>%
    here(PE.cfg$dir$scratch,.,PE.cfg$file$config) 
  assert_that(file.exists(cfg.fname),
              msg=sprintf("Cannot find configuration file, %s.",cfg.fname))
  readRDS(cfg.fname)
}




