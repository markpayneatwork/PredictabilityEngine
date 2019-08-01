#' PredEng Project configuration class
#'
#' @slot project.name Name of the configuration
#' @slot recalculate Should all existing files be recalculated? Or only missing files?
#' @slot Observations A data.source object defining the observational dataset to include. 
#' @slot Decadal A list of GCM objects defining the decadal forecast systems to be analysed
#' @slot NMME A list of GCM objects defining the NMME models to be analysed
#' @slot CMIP5 A data source object to configure extraction from the CMIP5 ensemble. 
#' @slot spatial.subdomains List of spatial.config objects defining the spatial subdomains over which 
#' to operate
#' @slot statistics PredEng.list of statistics to apply over each spatial area
#' @slot extraction PredEng.list definining temporal and spatial extraction characteristics
#' @slot MOI The months of interest (a vector of integers between 1 and 12 inclusive)
#' @slot clim.years The years to include in the climatology and analysis of hindcast skill (vector of 
#' integers)
#' @slot comp.years The years over which to make comparisons between observations and models
#' @slot landmask The file to use as the basis for masking out land [optional]
#' @slot scratch.dir Directory in which processed files are to be stored
#' @slot global.ROI  Extent object defining the global ROI to work with. 
#' @slot global.res Resolution of the analysis to be applied globally
#' @slot use.global.ROI Logical to indicator whether we generate a combined set of files, or one set 
#' for each area.
#' @slot retain.realizations Is there interest in retaining the individual realisations from each model, or
#' should we go straight to the realisation means (as a way of saving disk space and simiplifying the
#' processing)?
#' @slot average.months Should we average over the months of interest?
#'
#' @include Data_class_and_methods.r
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
PredEng.config <- setClass("PredEng.config",
                           slots=list(project.name="character",
                                      recalculate="logical",
                                      Observations="data.source",
                                      Decadal="list",
                                      NMME="list",
                                      CMIP5="data.source",
                                      spatial.subdomains="list",
                                      statistics="list",
                                      extraction="list",
                                      global.ROI="Extent",
                                      global.res="numeric",
                                      MOI="numeric",
                                      clim.years="numeric",
                                      comp.years="numeric",
                                      landmask="character",
                                      scratch.dir="character",
                                      use.global.ROI="logical",
                                      retain.realizations="logical",
                                      average.months="logical"),
                           prototype = list(global.ROI=extent(as.numeric(rep(NA,4))),
                                            recalculate=TRUE,
                                            retain.realizations=TRUE))





#'========================================================================
# Misc ####
#'========================================================================

#' #' Merge the regions of interest stored in the config object into one
#' #' 
#' #' 
#' setMethod("merge",signature(x="project.config",y="missing"),
#'           function(x,y,..){
#'             #Merge Regions of interest
#'             poly.ROIs <- lapply(x@indicators,slot,"poly.ROI")
#'             ext.ROIs <- lapply(poly.ROIs,extent)
#'             if(length(ext.ROIs)>1) {
#'               ROI <- do.call(raster::merge,unname(ext.ROIs))
#'             } else {
#'               ROI <- ext.ROIs[[1]]
#'             }
#'             return(ROI)
#'           })

#' #' Visualise project region of interest
#' #' @export
#' setMethod("plot",signature(x="project.config",y="missing"),
#'           function(x,y,...) {
#'             require(maps)
#'             #Try to plot a map as background first
#'             err <- try(map("world",xlim=x@ROI[1:2],ylim=x@ROI[3:4],
#'                            fill=TRUE,col="black"),silent=TRUE)
#'             if(is(err,"try-error")) {
#'               plot(x@ROI,lwd=2,col="grey",...)
#'             } else {
#'               plot(x@ROI,lwd=2,col="grey",add=TRUE,...)
#'             }
#'             #Add individual areas
#'             for(i in seq(x@indicators)) {
#'               plot(x@indicators[[i]]@poly.ROI,border=i,add=TRUE)
#'             }
#'           })


#' @export
setMethod("show","PredEng.config", function(object) {
  hdr.str <- paste( object@project.name,"configuration")
  cat(hdr.str,"\n")
  cat(paste(rep("-",nchar(hdr.str)),collapse="",sep=""),"\n")
  show.slot <- function(ob,slt) {
    obj <- slot(ob,slt)
    if(class(obj) %in% c("logical","formula","character",
                         "numeric","Extent","integer","list",
                         "data.source")) {
      cat(sprintf("%-20s : ",slt))
    } else {return(NULL)}
    if(is(obj,"formula")) {
      cat(deparse(obj,width.cutoff=options()$width),"\n")
    } else if(is(obj,"Extent")){
      cat(paste(obj[],collapse=", "),"\n")
    } else if(is(obj,"numeric") & length(obj) ==1){
      cat(obj,"\n")
    } else if(is(obj,"numeric") & length(obj) >=12){
      cat(paste(range(obj),collapse="-"),"\n")
    } else if(is(obj,"numeric") & length(obj) <12){
      cat(paste(range(obj),collapse="-"),"\n")
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

