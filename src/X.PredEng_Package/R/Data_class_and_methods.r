# ========================================================================
#  Classes for storing metadata about data sources
# ========================================================================
#' Metadata class for describing data sources compatabile with PredEng
#'
#' @slot name Identifier of data source 
#' @slot type Type of data source that this object corresponds to - valid 
#' options are "Decadal", "NMME", "CMIP","Obs", and eventually "C3S" 
#'
#' @export PredEng.source
#' @exportClass PredEng.source
PredEng.source <- setClass("PredEng.source",slots=list(name="character",
                                           type="character"))


#' Metadata class for describing data from a single model 
#'
#' @slot name Identifier of data source 
#' @slot type Type of data source that this object corresponds to - valid 
#' options are "Decadal", "NMME", "CMIP","Obs", and eventually "C3S" 
#' @slot source Directory or URL containing the raw information
#' @slot var Variable name from which to extract data
#' @slot levels Level(s) in the source data set containing the relevant data
#' @slot realizations Realization(s) to use. Zero indicates all.
#' @slot ensmem_fn A function to extract the realisation, for cases where a simple index doesn't work
#' @slot init_fn Function to extract the initialisation dates
#' @slot date_fn A function to extract the time stamps for each time step
#'
#' @export data.source
#' @exportClass data.source
data.source <- setClass("data.source",contains="PredEng.source",
                        slots=list(source="character",
                                   var="character",
                                   levels="numeric",
                                   realizations="numeric",
                                   ensmem_fn="function",
                                   init_fn="function",  #TODO: Consider dropping this slot
                                   date_fn="function"),
                        prototype=list(levels=1,
                                       realizations=0,
                                       date_fn=function(f) {
                                         if(length(f)>1) stop("Function not vectorised")
                                         dates <- getZ(brick(f))
                                         return(dates)
                                       }))

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
    } else if(is(obj,"list") ){
      cat(paste(names(obj),collapse=", "),"\n")
    } else {
      cat(paste(obj,collapse=", "),"\n")
    }
  }
  
  for(i in slotNames(object)) {show.slot(object,i)}

})

