# Base stats class ========================================================================

#' Statistics class
#'
#' @param name Name of the statistics class. Cannot contain spaces, underscores or dots. Must be less than 20 char
#' @param desc Description of the statistics class. 
#' @param realizations Tells what sort of data to be considered  1 = Observations for Persistence 
#' 2 = Individual realisations  3 = Realization means 4 = Ensemble means. Observations are always calculated.
#' @param calibration Choose the calibration method to base the statistic on. Defaults to NULL, indicating
#' to use all available calibrations
#' @param use.globalROI Indicates whether the stat should also be calculated on global basis, in addition
#' to those specified by this.stat@spatial.polygons slot.
#' @param spatial.polygons The names of spatial polygons to run this metric with. Setting this to 
#' NULL (the default) will use all available polygons. NA will use none.
#' @param retain.field Should fields generated by the metric be retained in the output? 
#' This can be expensive in terms of storage, so it can be turned off here.
#' @name stat
#' @export stat
#' @exportClass stat
stat <- 
  setClass("stat",
           slots=list(name="character",
                      desc="character",
                      realizations="numeric",
                      calibration="character",
                      use.globalROI="logical",
                      spatial.polygons="character",
                      retain.field="logical")
,
           prototype = list(realizations=1:4,  #Default to all
                            retain.field=FALSE,
                            use.globalROI=FALSE),
           validity = function(object) {
             err.msg <- list(
               validate_that(!grepl(" ",object@name),msg="Object name must not contain spaces"),
               validate_that(!grepl("\\.",object@name),msg="Object name must not contain full stops"),
               validate_that(!grepl("_",object@name),msg="Object name must not contain underscores"),
               validate_that(nchar(object@name)<20,msg="Object name must not exceed 20 characters"),
               validate_that(!is_empty(object@desc),msg=".description slot must not be empty"),
               validate_that(!is_empty(object@name),msg=".@name slot must not be empty"),
               validate_that(all(object@calibration %in% PE.cfg$validity$calibrationMethod) | length(object@calibration)==0,
                             msg="Unsupported calibration method selected"),
               validate_that(!(any(is.na(object@spatial.polygons)) & !object@use.globalROI),
                             msg="No spatial polygons selected by this stat. Specify a polygon or use the global ROI."),
               validate_that(all(object@realizations %in% 1:4),
                             msg="Valid choices for realization are 1, 2, 3 and 4"))
             err.idxs <- map_lgl(err.msg,is.character)
             if(all(!err.idxs)) return(TRUE) else unlist(err.msg[err.idxs])
           })

#' Evaluate an stat
#' @export
setGeneric("eval.stat",
           function(st,dat)
             standardGeneric("eval.stat"))

#' Return type
#' @export
setGeneric("returns.field",
           function(st)
             standardGeneric("returns.field"))

#' @export
#Defaults to retain value
setMethod("returns.field",signature(st="stat"),
          function(st){
            return(st@retain.field)
          })

#' Return type
#' @export
setGeneric("returns.scalar",
           function(st)
             standardGeneric("returns.scalar"))

#' @export
#Defaults to TRUE
setMethod("returns.scalar",signature(st="stat"),
          function(st){
            return(TRUE)
          })


# Thresholds ========================================================================

#' Thresholds
#'
#' Determines where each pixel sits in relation to a threshold value and integrates  over the
#' area of interest to get the area satisfying the threshold
#' @param thre shold Critical threshold value - a numeric of length 1 or 2
#' @param above Logical value - TRUE indicates that we wish to test for values above the threshold. FALSE below. 
#' Ignored if two thresholds are supplied.
#' @export threshold
#' @return  \code{threshold} returns a tibble containing the Raster* object matching the raster object supplied as an
#' argument. If two thresholds are supplied, the raster corresponds to that between the two thresholds
threshold <- 
  setClass("threshold",
           slots=list(threshold="numeric",
                      above="logical"),
           prototype=list(name="threshold",
                          above=TRUE),
           contains="stat",
           validity = function(object) {
             err.msg <- list(
               validate_that(length(object@threshold) %in% c(1,2),
                             msg="Length of 'threshold' slot should be 1 or 2"))
             err.idxs <- map_lgl(err.msg,is.character)
             if(all(!err.idxs)) return(TRUE) else unlist(err.msg[err.idxs])
           })

#' @export
setMethod("eval.stat",signature(st="threshold",dat="Raster"),
          function(st,dat){
            
            #Apply thresholding
            if(length(st@threshold)==2) {
              pass.threshold <- dat > min(st@threshold) & dat < max(st@threshold)
            } else if(st@above) {
              pass.threshold <- dat > st@threshold
            } else {
              pass.threshold <- dat < st@threshold
            }
            res.field <- 
              tibble(resultName="field",
                     field=purrr::map(1:nlayers(pass.threshold),
                                      function(i) pass.threshold[[i]]))
            
            #Now calculate the area
            pxl.area <- area(dat)
            area.masked <- pxl.area * pass.threshold
            names(area.masked) <- names(dat)
            area.satistfying.thresh <- cellStats(area.masked,sum)
            
            #Filter areas where it doesn't work.
            mean.temp <- cellStats(dat,mean)
            area.filt <- ifelse(is.na(mean.temp),NA,area.satistfying.thresh)
            res.value <- tibble(resultName="area",
                                 value=area.filt)
            
            #Return
            if(st@retain.field) {
              return(bind_rows(res.field,res.value)) 
            } else {
              return(res.value)
            }
          })



# Averaging ========================================================================


#' Average value within an ROI
#'
#' Calculates the average value of a variable within a region of interest using
#' area-weighting
#' @export spatial.mean
spatial.mean <- 
  setClass("spatial.mean",contains="stat",
           prototype=list(name="spatial.mean"))

#' @export
setMethod("eval.stat",signature(st="spatial.mean",dat="Raster"),
          function(st,dat) {
            #Crop supplied object to the spatial polygon and then mask
            #b.crop <- crop(x,m@poly.ROI)
            #b <- mask(r,sp@boundary)
            
            #Get pixel area
            pxl.area <- area(dat)
            
            #Calculate the terms in the weighted average
            temp.by.area <- dat*pxl.area
            na.by.area   <- (!is.na(dat))*pxl.area
            wt.temp <- cellStats(temp.by.area,sum)/cellStats(na.by.area,sum)
            
            return(tibble(resultName="average",
                          field=NA,
                          value=wt.temp))
          })


setMethod("returns.field",signature(st="spatial.mean"),
          function(st){
            return(FALSE)
          })


# Pass through ========================================================================

#' Pass-through statistic
#'
#' Returns the field that was supplied. This can be useful, for example, if we
#' want to look at anomaly correlation coefficients and just want to pass the 
#' value straight through into the statistics processing and skill metric system
#' @export pass.through
pass.through <- 
  setClass("pass.through",contains="stat",
           prototype=list(name="pass.through"))

#' @export
setMethod("eval.stat",signature(st="pass.through",dat="Raster"),
          function(st,dat) {
            return(tibble(resultName="field",
                          value=NA,
                          field=list(dat)))
          })

setMethod("returns.field",signature(st="pass.through"),
          function(st){
            return(TRUE)
          })

setMethod("returns.scalar",signature(st="pass.through"),
          function(st){
            return(FALSE)
          })


# Custom Stat ========================================================================

#' Custom Stat 
#'
#' Applies a custom function on a pixel-by-pixel basis, potentially integrating it to a scalar
#' response variable. The responsibility for the implementation of this model lies with the user - the only
#' check made here is that the function returns a tibble with columns "resultName", "field" and "value"
#' @name custom.stat
#' @export custom.stat
custom.stat<- 
  setClass("custom.stat",
           slots=list(fn="function",
                      resources="list"),
           prototype=list(name="custom.stat"),
           contains="stat",
           validity = function(object) {
             err.msg <- list(
               validate_that(identical(names(formals(object@fn)),c("dat","resources")),
                             msg='Function in fn slot must have exactly two arguments: "dat" and "resources"'))
             err.idxs <- map_lgl(err.msg,is.character)
             if(all(!err.idxs)) return(TRUE) else unlist(err.msg[err.idxs])
           })

setMethod("eval.stat",signature(st="custom.stat",dat="Raster"),
          function(st,dat){
            
            #Apply the custom model
            this.res <- st@fn(dat,st@resources)
            
            #Check results are valid
            assert_that(is.data.frame(this.res),
                        msg="Result from custom function must be a data.frame or tibble")
            ok.colnames <- c("resultName","field","value")
            assert_that(identical(names(this.res),ok.colnames),
                        msg=sprintf("Results from custom function must have column names `%s`",
                                    paste(ok.colnames,collapse=", ")))
            
            #Drop fields forcibly
            if(!st@retain.field) {
              this.res <- filter(this.res,is.na(field))
            }
            
            #Return
            return(this.res)})


