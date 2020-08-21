# ========================================================================
#  Classes for storing metadata about data sources
# ========================================================================
#' Metadata class for describing data from a single model 
#'
#' @slot name Identifier of data source 
#' @slot type Type of data source that this object corresponds to - valid 
#' options are "Decadal", "NMME", "CMIP","Obs", and eventually "C3S" 
#' @slot sources A vector of directories or URLs containing the raw information. Each item in the list
#' corresponds to a processing chunk. 
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
#' are initialised monthly. May be deprecated at some point
#' @slot date.fn A function to extract the time stamps for each time step
#' @slot crs Specify the CRS. If empty, then take the CRS from the input file (where raster can get it)
#'
#' @export data.source
#' @exportClass data.source
data.source <- 
  setClass("data.source",
           slots=list(name="character",
                      type="character",
                      sources="character",
                      var="character",
                      time.var="character",
                      realizations="character",
                      use.timebounds="numeric",
                      realization.fn="function",
                      layermids.fn="function",
                      start.date="function", 
                      date.fn="function",
                      crs="CRS"),
           prototype=list(use.timebounds=as.numeric(NA),
                          time.var="time",
                          layermids.fn=function(x) {stop("z2idx.fn not specified")},
                          realizations=as.character(NA),
                          date.fn=function(x) {stop("Date.fn not specified")},
                          realization.fn=function(x) {stop("Realization function not specified")},
                          start.date=function(x) { stop("Start.date function not specified")},
                          crs=CRS()),
           validity = function(object) {
             err.msg <- 
               list(validate_that(!grepl(" ",object@name),msg="Object name must not contain spaces"),
                    validate_that(!grepl("-",object@name),msg="Object name must not contain hyphens"),
                    validate_that(!grepl("_",object@name),msg="Object name must not contain underscores"),
                    validate_that(is.na(object@use.timebounds) | object@use.timebounds %in% 1:3,
                                  msg="Use.timebounds must be either NA, or 1,2 or 3"))
             err.idxs <- map_lgl(err.msg,is.character)
             if(all(!err.idxs)) return(TRUE) else unlist(err.msg[err.idxs])
           })


#' @export
setMethod("show","data.source", function(object) {
  hdr.str <- paste(class(object))
  cat(hdr.str,"\n")
  cat(paste(rep("-",nchar(hdr.str)),collapse="",sep=""),"\n")
  show.slot <- function(ob,slt) {
    obj <- slot(ob,slt)
    if(class(obj) %in% c("logical","formula","character","numeric","Extent","integer","list","CRS")) {
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

