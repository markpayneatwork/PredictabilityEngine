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
#' @slot z2idx Converts a range of vertical depths (postive downwards) to the corresponding layers. Should take
#' the arguments z (a length=2 vector of depths) and f (a link to a file). Returns a list of levels that closest
#' approximate the range
#' @slot date.fn Given a filename, f, returns the corresponding date
#' @slot start.date Function to extract the dates for the start of the forecasts. Note that we define this to
#' be different to the initialisation dates (when the model is actually initialised). A decadal model 
#' might be initialised on 1 Nov but we are primarily interested in it's post-January output, and so call 
#' this an effective start date of 1 Jan. Things are much clearer for seasonal forecast systems, that 
#' are initialised monthly. May be deprecated at some point
#' @slot date.fn A function to extract the time stamps for each time step
#'
#' @export data.source
#' @exportClass data.source
data.source <- 
  setClass("data.source",
           slots=list(name="character",
                      type="character",
                      sources="character",
                      var="character",
                      fields.are.2D="logical",
                      time.var="character",
                      realizations="character",
                      use.timebounds="numeric",
                      realization.fn="function",
                      z2idx="function",
                      start.date="function", 
                      date.fn="function"),
           prototype=list(use.timebounds=as.numeric(NA),
                          time.var="time",
                          realizations=as.character(NA),
                          date.fn=function(f) {stop("Date.fn not specified")},
                          z2idx=function(z,f) {stop("z2idx function not specified")},
                          realization.fn=function(x) {stop("Realization function not specified")},
                          start.date=function(x) { stop("Start.date function not specified")}),
           validity = function(object) {
             err.msg <- 
               list(validate_that(!grepl(" ",object@name),msg="Object name must not contain spaces"),
                    #validate_that(!grepl("-",object@name),msg="Object name must not contain hyphens"),
                    validate_that(!grepl("_",object@name),msg="Object name must not contain underscores"),
                    validate_that(is.na(object@use.timebounds) | object@use.timebounds %in% 1:3,
                                  msg="Use.timebounds must be either NA, or 1,2 or 3"),
                    validate_that(identical(names(formals(object@date.fn)),"f"),
                                  msg="Function in 'date.fn' slot must only take argument 'f'"),
                    validate_that(identical(names(formals(object@z2idx)),c("z","f")),
                                  msg="Function in 'z2idx' slot must only take arguments 'z' and 'f'"))
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


#' Convert vertical coordinates to layer indices
#'
#' bounds.to.indices converts a vertical range (two elements) to the indices of the vertical levels defined by their
#' upper and lower bounds. A layer is included if the bound includes more than 50% of it
#'
#' @param z.range the vertical range
#' @param layer.top a vector defining the depths of the tops of the layers
#' @param layer.bottom a vector defining the depths of the bottoms of the layers
#'
#' @return
#' @name verticalLayers
#' @export
bounds.to.indices <- function(z.range,layer.top,layer.bottom) {
  #Check inputs
  assert_that(length(z.range)==2,msg="z.range needs to be of length two")
  assert_that(length(layer.top)==length(layer.bottom),
              msg="Size of 'upper.bnd'layer.top' and 'layer.bottom' vectors must be the same")
  #Apply logic
  keep.layers <-  
    tibble(layer.top,
           layer.bottom,
           layer.thickness=-(layer.top-layer.bottom),
           zmin.rel.pos=(min(z.range)-layer.top)/layer.thickness,
           zmin.ok=round(zmin.rel.pos)<=0,
           zmax.rel.pos=(max(z.range)-layer.top)/layer.thickness,
           zmax.ok=round(zmax.rel.pos)>=1) %>%
    mutate(idx=1:nrow(.)) %>%
    filter(zmin.ok & zmax.ok)
  assert_that(nrow(keep.layers)>0,msg="Problem with z.range specification")
  return(keep.layers$idx)
}


#' 
#' 
#' midpoints.to.indices works with midpoints of the layers, instead of bounds
#' 
#' @param midpoints A vector of the midpoints
#'
#' @return
#' @export
#' @rdname verticalLayers
midpoints.to.indices <- function(z.range,midpoints) {
  #We take a nearest neighbour approach. This is not perfect, but in most cases it will be ok
  #In particular, as the midpoint of the cell is at 0.5, then we can take advantage of this 
  #as the inclusion / exclusion criteria, depending on the direction
  idx.min <- which.min(ifelse(min(z.range)>midpoints,NA,midpoints))  #Round up
  idx.max <- which.max(ifelse(max(z.range)>midpoints,midpoints,NA)) #Round down
  return(c(idx.min,idx.max))
}


#' Test Data Source
#'
#' Tests the metadata extraction functions in an object. Useful for testing and developing code.
#'
#' @param obj Data.source object
#' @param z.range Range of vertical coordinates over which to test the z2idx function
#' @param f  Filename on which to test the functions. If missing, defaults to the first file in the data source list
#' @return Displays the outputs generated by the functions
#' @export
#'
test.data.source <- function(obj,z.range=c(250,600),f="missing"){
  if(missing("f")) { f <- unlist(obj@sources)[1]}
  show(obj)
  log_msg("\nTest file : %s\n",f)
  log_msg("\nDates\n")
  print(obj@date.fn(f))
  log_msg("\nRealizations\n")
  print(obj@realization.fn(f))
  log_msg("\nEffective Forecast Start Date\n")
  print(obj@start.date(f))
  if(!obj@fields.are.2D) {
    log_msg("\nVertical z2idx test\n")
    print(obj@z2idx(z.range,f))
  } else {
    log_msg("\nFields are 2D - no vertical test.")
  }
}

#' Extract data.source objecct
#'
#' Extract the correct datasource object from the configuration specifying the
#' source type and source name. Note that this includes both the model slot and
#' the observations slot.
#'
#' @param object PredEng.config object
#' @param this.srcType Type of the data.source to match
#' @param this.srcName Name of the data.source to match
#'
#' @return data.source object
#' @export
PE.get.datasrc <- function(object,this.srcType,this.srcName) {
  datasrc.sel <- 
    tibble(data.src=c(object@Models@.Data,object@Observations)) %>%
    mutate(srcName=map_chr(data.src,slot,"name"),
           srcType=map_chr(data.src,slot,"type")) %>%
    filter(srcName==this.srcName,
           srcType==this.srcType) 
  assert_that(nrow(datasrc.sel)==1,msg="Error identifying a unique data source")
  return(datasrc.sel$data.src[[1]])
}



