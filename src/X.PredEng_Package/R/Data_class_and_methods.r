# ========================================================================
#  Classes for storing metadata about data sources
# ========================================================================
#' Metadata class for describing data from a single model 
#'
#' @slot name Identifier of data source 
#' @slot type Type of data source that this object corresponds to - valid 
#' options are "Decadal", "NMME", "CMIP","Obs", and eventually "C3S" 
#' @slot n.chunks Numeric. (Max) number of chunks allocated to processing this data source. 
#' @slot chunk.id Chunk identifier (optional)
#' @slot sources A list of directories or URLs containing the raw information. Each item in the list
#' corresponds to a processing chunk. The number of chunks here might be less than n.chunks, but
#' should not exceed it
#' @slot var Variable name from which to extract data
#' @slot realizations Character string, naming the realization(s) to use. NA indicates all.
#' @slot use.timebounds Indicates whether to use timebounds instead of "time" variable, when selecting
#' by time. Valid values are NA (don't use timebounds, use the time variable), 1 (use the lower bound),
#' 2 (use the upper bound) or 3 (use the average value)
#' @slot realization.fn A function to extract the realisation ID
#' @slot layermids.fn Returns the midpoints of the vertical layers for this data set. 
#' @slot start.date Function to extract the dates for the start of the forecasts. Note that we define this to
#' be different to the initialisation dates (when the model is actually initialised). A decadal model 
#' might be initialised on 1 Nov but we are primarily interested in it's post-January output, and so call 
#' this an effective start date of 1 Jan. Things are much clearer for seasonal forecast systems, that 
#' are initialised monthly.
#' @slot date.fn A function to extract the time stamps for each time step
#'
#' @export data.source
#' @exportClass data.source
data.source <- 
  setClass("data.source",
           slots=list(name="character",
                      type="character",
                      n.chunks="numeric",
                      chunk.id="character",
                      sources="list",
                      var="character",
                      time.var="character",
                      realizations="character",
                      use.timebounds="numeric",
                      realization.fn="function",
                      layermids.fn="function",
                      start.date="function",  
                      date.fn="function"),
           prototype=list(use.timebounds=as.numeric(NA),
                          time.var="time",
                          n.chunks=1,
                          chunk.id="",
                          layermids.fn=function(x) {stop("z2idx.fn not specified")},
                          realizations=as.character(NA),
                          date.fn=function(x) {stop("Date.fn not specified")},
                          realization.fn=function(x) {stop("Realization function not specified")},
                          start.date=function(x) { stop("Start.date function not specified")}),
           validity = function(object) {
             err.msg <- NULL
             if(grepl(" ",object@name)) {
               err.msg <- c(err.msg,"Object name must not contain spaces.")}
             if(grepl("\\.",object@name)) {
               err.msg <- c(err.msg,"Object name must not contain full stops.")}
             #Check names on sources
             src.names <- names(object@sources)
             if(length(unique(src.names))!=length(object@sources) & length(src.names)>1)
               if(length(slot(object,n))==0) {
                 err.msg <- c(err.msg,"List of sources must be named with unique names")
               }
             #Check use.timebounds is value
             if(!(is.na(object@use.timebounds) | (object@use.timebounds %in% 1:3))) {
               err.msg <- c(err.msg,"Use.timebounds must be either NA, or 1,2 or 3")
             }
             if(length(err.msg)==0) return(TRUE) else err.msg
           })


#' @export
setMethod("show","data.source", function(object) {
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


#' Chunk data source
#' 
#' Splits an existing data source object into equally sized chunks
#'
#' @param obj Data source object
#' @param n Number of chunks into which to split the object
#'
#' @return A data source object configured for use as chunks
#' @export
#'
chunk.data.source <- function(obj,n=1) {
  obj@n.chunks <- n
  src.fnames <- unlist(obj@sources)
  if(length(src.fnames!=0)) {
    obj@sources <- split(src.fnames,rep(1:n,length.out=length(src.fnames)))
    names(obj@sources) <- sprintf("Chunk_%03i",seq(length(obj@sources)))
  }
  return(obj)
}



#' Timebounds to Time var
#' 
#' In cases where a netcdf file has time boundaries associated with it, it can
#' be useful to copy one of these values into the time variable, to facilitate
#' easier processing.
#'
#' @param this.obj A data source object
#' @param f The path of the variable to be modified
#'
#' @export
timebounds.to.time <- function(this.obj,f){
  #Open file
  ncid <- nc_open(f,write=TRUE)
  #Get bounds values
  bnds.var <- ncatt_get(ncid,this.obj@time.var,"bounds")$value
  bnds.vals <- ncvar_get(ncid,bnds.var)
  #Calculate average
  bnds.vals <- rbind(bnds.vals,colMeans(bnds.vals))
  #Write new value
  ncvar_put(ncid,this.obj@time.var,vals=bnds.vals[this.obj@use.timebounds,])
  nc_close(ncid)
  return(invisible(NULL))
}


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
  if(missing("f")) { f <- unlist(obj@sources)[1]}
  show(obj)
  log_msg("\nTest file : %s\n",f)
  log_msg("\nDates\n")
  print(obj@date.fn(f))
  log_msg("\nRealizations\n")
  print(obj@realization.fn(f))
  log_msg("\nEffective Forecast Start Date\n")
  print(obj@start.date(f))
}

