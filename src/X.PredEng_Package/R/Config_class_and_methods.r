#' PredEng Project configuration class
#'
#' @slot project.name Name of the configuration
#' @slot Observations A data.source object defining the observational dataset to include. 
#' @slot Decadal A list of GCM objects defining the decadal forecast systems to be analysed
#' @slot NMME A list of GCM objects defining the NMME models to be analysed
#' @slot CMIP5 A list of data source objects to configure extraction from the CMIP5 ensemble. 
#' @slot persistence.leads A vector (in months) of lead times at which to generate persistence forcasts
#' @slot spatial.polygons A sf data.frame defining the spatial domains over which  to operate
#' @slot statistics PredEng.list of statistics to apply over each spatial area
#' @slot extraction PredEng.list definining temporal and spatial extraction characteristics
#' @slot MOI The months of interest (a vector of integers between 1 and 12 inclusive)
#' @slot vert.range The vertical range, in m, over which to average. NA indicates no vertical averaging (for 
#' 2D fields) or to use the surface layer (for 3D fields). 
#' @slot clim.years The years to include in the climatology and analysis of hindcast skill (vector of 
#' integers)
#' @slot comp.years The years over which to make comparisons between observations and models
#' @slot landmask The file to use as the basis for masking out land [optional]
#' @slot scratch.dir Directory in which processed files are to be stored
#' @slot global.ROI  Extent object defining the global ROI to work with. 
#' @slot global.res Resolution of the analysis to be applied globally
#' @slot retain.realizations Is there interest in retaining the individual realisations from each model, or
#' should we go straight to the realisation means (as a way of saving disk space and simiplifying the
#' processing)?
#' @slot average.months Should we average over the months of interest?
#'
#' @include Data_class_and_methods.r
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
                      Observations="data.source",
                      Decadal="PElst",
                      NMME="PElst",
                      CMIP5="PElst",
                      persistence.leads="numeric",
                      spatial.polygons="sf",
                      statistics="PElst",
                      #extraction="list",
                      global.ROI="Extent",
                      global.res="numeric",
                      MOI="numeric",
                      vert.range="numeric",
                      clim.years="numeric",
                      comp.years="numeric",
                      landmask="character",
                      scratch.dir="character",
                      retain.realizations="logical",
                      average.months="logical"),
           prototype = list(global.ROI=extent(as.numeric(rep(NA,4))),
                            persistence.leads=1:120,  #1-10 years
                            retain.realizations=TRUE,
                            vert.range=as.numeric(NA),  #Use surface unless specified
                            spatial.polygons=st_sf(st_sfc(st_point(c(0,0))))),
           validity = function(object) {
             #Basics
             err.msg <- list(
               validate_that(!(length(object@MOI)!=1 & object@average.months),
                             msg="Dates are currently not handled correctly when averaging over multiple months"),
               validate_that(length(object@vert.range)==2 | all(is.na(object@vert.range)),
                             msg="Vertical range slot must be of length 2 if not NA"))
             #Check for consistency between presence of 2D fields and requested vertical range
             datsrc.meta <- 
               tibble(data.srcs=c(object@Decadal,object@NMME,object@Observations,object@CMIP5)@.Data) %>%
               mutate(srcType=map_chr(data.srcs,slot,"type"),
                      srcName=map_chr(data.srcs,slot,"name"),
                      is.2D=map_lgl(data.srcs,slot,"fields.are.2D"))
             err.msg[["vertrange.2D"]] <-
               validate_that(all(is.na(object@vert.range)) | all(!datsrc.meta$is.2D),
                             msg="If vertical range is specified, all data sources must be 3D")
             
             err.idxs <- map_lgl(err.msg,is.character)
             if(all(!err.idxs)) return(TRUE) else unlist(err.msg[err.idxs])
           })



#' Visualise project region of interest
#' @export
setMethod("plot",signature(x="PredEng.config",y="missing"),
          function(x,y,...) {
            x@spatial.polygons %>%
            ggplot()+
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
  for(i in slotNames("PredEng.config")) {show.slot(object,i)}

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
#' @param pcfg 
#'
#' @return
#' @export
set.configuration <- function(pcfg) {
  #Check object is initially valid
  validObject(pcfg)

  #Set output directory
  define_dir(pcfg@scratch.dir)

  #Write CDO grid descriptors
  griddes.txt <- griddes(pcfg@global.ROI,res=pcfg@global.res)
  writeLines(griddes.txt,PE.scratch.path(pcfg,"analysis.grid"))

  #Write regridded landmask
  landmask.cmd <- cdo("--silent -f nc",
                      csl(" remapnn", PE.scratch.path(pcfg,"analysis.grid")),
                      pcfg@landmask,
                      PE.scratch.path(pcfg,"landmask"))
  #Output
  cfg.fname <- PE.scratch.path(pcfg,"config")
  cfg.linked <- PE.cfg$path$config
  saveRDS(pcfg,file=cfg.fname)
  cat(pcfg@project.name,file=file.path(PE.cfg$dir$objects,"configuration.name"))
  if(file.exists(cfg.linked)) {
    file.remove(cfg.linked)
  }
  file.symlink(file.path(getwd(),cfg.fname),PE.cfg$dir$objects)
  
  #Setup drake directory
  drake.link <- here(".drake")
  if(file.exists(drake.link)) {
    file.remove(drake.link)
  }
  file.symlink(define_dir(here(pcfg@scratch.dir,".drake")),drake.link)

  #Setup SQLite database to store results
  PE.db.setup(pcfg)
  
  # HPC  Configuration ####
  # #Setup soft linking
  # project.cfg <- define_dir(pcfg@scratch.dir,basename(PE.cfg$dirs$job.cfg))
  # unlink(PE.cfg$dirs$job.cfg)
  # file.symlink(file.path(getwd(),project.cfg),PE.cfg$dirs$job.cfg)
  # 
  # #Need a TODO directory as well
  # TODO.dir <- define_dir(PE.cfg$dirs$job.cfg,"TODO")
  # 
  # #Write configurations
  # list(NMME=c("Sources","Ensmean"),
  #      Decadal=c("Chunks","Sources","Ensmean"),
  #      CMIP5=c("Sources"),
  #      Observations=NA) %>%
  #   enframe("src.slot","data.partition.type") %>%
  #   unnest() %>%
  #   pwalk(partition.workload,obj=pcfg)

  # cfgs <- partition.workload(pcfg,"NMME","Sources")
  # cfgs <- partition.workload(pcfg,"NMME","Ensmean")
  # cfgs <- partition.workload(pcfg,"Decadal","Chunks")
  # cfgs <- partition.workload(pcfg,"Decadal","Sources")
  # cfgs <- partition.workload(pcfg,"Decadal","Ensmean")
  # cfgs <- partition.workload(pcfg,"CMIP5","Sources")
  # cfgs <- partition.workload(pcfg,"CMIP5","Ensmean")
  # cfgs <- partition.workload(pcfg,"Observations")

  #Final check
  validObject(pcfg,complete=TRUE)

  return(pcfg)
}




