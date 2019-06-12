# ========================================================================
#  Classes for storing metadata about data sources
# ========================================================================
#' Metadata class for describing data sources compatabile with PredEng
#'
#' @slot name Name of data source e.g. the model that it derives from
#' @slot type Type of data source that this object corresponds to - valid 
#' options are "Decadal", "NMME", "CMIP","Observations", and eventually "C3S" 
#'
#' @export PredEng.source
#' @exportClass PredEng.source
PredEng.source <- setClass("PredEng.source",
                           slots=list(name="character",
                                      type="character"),
                           validity = function(object) {
                             err.msg <- NULL
                             for(n in slotNames(object)) {
                               if(length(slot(object,n))==0) {
                                 err.msg <- c(err.msg,
                                              sprintf('Slot "%s" is undefined.',n))
                               }
                             }
                             if(length(err.msg)==0) return(TRUE) else err.msg
                           })


#' Metadata class for describing data from a single model 
#'
#' @slot name Identifier of data source 
#' @slot type Type of data source that this object corresponds to - valid 
#' options are "Decadal", "NMME", "CMIP","Obs", and eventually "C3S" 
#' @slot source Directory or URL containing the raw information
#' @slot n.chunks Number of chunks into which to split the processing
#' @slot var Variable name from which to extract data
#' @slot levels Level(s) in the source data set containing the relevant data. A value of NA means that averaging 
#' take place over all available layers. Also an epic track by Avicii (RIP).
#' @slot realizations Character string, naming the realization(s) to use. NA indicates all.
#' @slot time.correction A string suitable for use with the "cdo shifttime,x" command string that can be used to 
#' correcct for time-bound versus time value problems.
#' @slot realization.fn A function to extract the realisation ID
#' @slot init.fn Function to extract the initialisation dates
#' @slot date.fn A function to extract the time stamps for each time step
#'
#' @export data.source
#' @exportClass data.source
data.source <- setClass("data.source",
                        contains="PredEng.source",
                        slots=list(source="character",
                                   n.chunks="numeric",
                                   var="character",
                                   levels="numeric",
                                   realizations="character",
                                   time.correction="character",
                                   realization.fn="function",
                                   init.fn="function",  #TODO: Consider dropping this slot
                                   date.fn="function"),
                        prototype=list(levels=1,
                                       n.chunks=1,
                                       realizations=as.character(NA),
                                       date.fn=function(x) {stop("Date.fn not specified")},
                                       realization.fn=function(x) {stop("Realization function not specified")},
                                       init.fn=function(x) { stop("Init.fn not specified")}))


#' @export
setMethod("show","PredEng.source", function(object) {
  hdr.str <- paste(class(object))
  cat(hdr.str,"\n")
  cat(paste(rep("-",nchar(hdr.str)),collapse="",sep=""),"\n")
  show.slot <- function(ob,slt) {
    obj <- slot(ob,slt)
    if(class(obj) %in% c("logical","formula","character","numeric","Extent","integer","list")) {
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
    } else if(is(obj,"character") & length(obj) ==1){
      cat(obj,"\n")
    } else if(is(obj,"character") & length(obj) >=1){
      cat(paste(length(obj),"items"),"\n")
    } else if(is(obj,"list") ){
      cat(paste(names(obj),collapse=", "),"\n")
    } else {
      cat(paste(obj,collapse=", "),"\n")
    }
  }
  
  for(i in slotNames(object)) {show.slot(object,i)}

})

#' Test Data Source
#'
#' Tests the metadata extraction functions in an object. Useful for testing and developing code.
#'
#' @param obj Data.source object
#' @param f  Filename on which to test the functions. If missing, defaults to the first file in the data source list
#'
#' @return Displays the outputs generated by the functions
#' @export
#'
test.data.source <- function(obj,f="missing"){
  if(missing("f")) { f <- obj@source[1]}
  show(obj)
  log_msg("\nTest file : %s\n",f)
  log_msg("\nDates\n")
  print(obj@date.fn(f))
  log_msg("\nRealizations\n")
  print(obj@realization.fn(f))
  log_msg("\nForecast Initialisation\n")
  print(obj@init.fn(f))
}

#' Data source chunk
#' 
#' A data chunk based on a subset of a full data.source object
#'
#' @slot chunk.id numeric. Unique identifier
#' 
#' @export data.source.chunk
#' @exportClass data.source.chunk
#' 
data.source.chunk <- setClass("data.source.chunk",
                        contains="data.source",
                        slots=list(chunk.id="character"),
                        prototype=list(chunk.id=as.character(NULL)))
