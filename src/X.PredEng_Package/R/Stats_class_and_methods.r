# ========================================================================
# Statistics classes and methods
# ========================================================================
#' Statistics class
#'
#' @param name Name of the statistics class
#' @param use.realmeans Indicates whether to use the mean of the individ realisations or
#' the realisation values themselves. This slot essentially acts as a flag telling
#' the script whether it wants 2D (lat-lon) or 3D (lat-lon-realization) data.
#' @param use.anomalies Should the statistic be calculated on the basis of
#' anomalies only or should the full field be used?
#' @param is.global.stat Indicates whether the stat should be calculated on a global or local basis ie. for 
#' each individual spatial domain, or once for the entire global ROI
#' @param skill.metrics A character vector listing the skill metrics to be applied for this statistic.
#' @name stat
#' @export stat
#' @exportClass stat
stat <- setClass("stat",
                     slots=list(name="character",
                                use.realmeans="logical",
                                use.anomalies="logical",
                                is.global.stat="logical",
                                skill.metrics="character"),
                     prototype = list(use.realmeans=TRUE,
                                      use.anomalies=FALSE,
                                      is.global.stat=FALSE))


#' Evaluate an stat
#' @export
setGeneric("eval.stat",
           function(st,vals, ...)
             standardGeneric("eval.stat")
)

#' Return type
#' @export
setGeneric("returns.field",
           function(st)
             standardGeneric("returns.field"))

#' Thresholds
#'
#' Determines where each pixel sits in relation to a threshold value, potentially integrating over the
#' area of interest
#' @param threshold Critical threshold value - a numeric of length 1 or 2
#' @param above Logical value - TRUE indicates that we wish to test for values above the threshold. FALSE below. 
#' Ignored if two thresholds are supplied.
#' @export threshold

#' @return  \code{threshold} returns a tibble containing the Raster* object matching the raster object supplied as an
#' argument. If two thresholds are supplied, the raster corresponds to that between the two thresholds
threshold <- setClass("threshold",
                                 slots=list(threshold="numeric",
                                            above="logical"),
                                 prototype=list(name="threshold",
                                                above=TRUE),
                                 contains="stat",
                                 validity = function(object) {
                                   err.msg <- NULL
                                   if(!length(object@threshold) %in% c(1,2)) {
                                     err.msg <- c(err.msg,
                                                  sprintf("Length of 'threshold' slot should be 1 or 2 but is %i.",length(object@threshold)))
                                   }
                                   if(length(err.msg)==0) return(TRUE) else err.msg
                                 })

#' @export
setMethod("eval.stat",signature(st="threshold",vals="Raster"),
          function(st,vals,...){
            return(threshold.fn(st,vals,integrate=FALSE))
          })

#' @export
setMethod("returns.field",signature(st="threshold"),
          function(st){
            return(TRUE)
          })


threshold.fn <- function(st,vals,integrate){
  
  if(length(st@threshold)==2) {
    pass.threshold <- vals > min(st@threshold) & vals < max(st@threshold)
  } else if(st@above) {
    pass.threshold <- vals > st@threshold
  } else {
    pass.threshold <- vals < st@threshold
  }
  
  if(integrate) {
    #Now calculate the area
    pxl.area <- area(vals)
    area.masked <- pxl.area * pass.threshold
    names(area.masked) <- names(vals)
    area.statistfying.thresh <- cellStats(area.masked,sum)
    
    #Filter areas where it doesn't work.
    mean.temp <- cellStats(vals,mean)
    area.filt <- ifelse(is.na(mean.temp),NA,area.statistfying.thresh)
    
    #Return
    return(tibble(realization=1:nlayers(vals),value=area.filt)) 
    
  } else { #Return the results
    return(tibble(field=list(pass.threshold)))
  }}

#' Threshold area
#' 
#' Returns the area of water defined by threshold(s)
#' 
#' @export area.threshold
#' @param threshold Critical threshold value(s) - a numeric of length 1 or 2
#' @param above Logical value - TRUE indicates that we wish to test for values above the threshold. FALSE below. 
#' If threshold is a length two vector, then this argument is ignored
#' @return  \code{threshold.area} returns a tibble  corresponding to the integrated area above (or below) the threshold value. 
#' If two thresholds are supplied, the integrated area corresponds to that between the two thresholds
area.threshold <- setClass("area.threshold",contains="threshold")

#' @export
setMethod("eval.stat",signature(st="area.threshold",vals="Raster"),
          function(st,vals,...){
            return(threshold.fn(st,vals,integrate=TRUE))
          })

setMethod("returns.field",signature(st="area.threshold"),
          function(st){
            return(FALSE)
          })


#' Average value within an ROI
#'
#' Calculates the average value of a variable within a region of interest using
#' area-weighting
#' @export spatial.mean
spatial.mean <- setClass("spatial.mean",contains="stat",
                         prototype=list(name="spatial.mean"))

#' @export
setMethod("eval.stat",signature(st="spatial.mean",vals="Raster"),
          function(st,vals,...) {
            require(reshape2)
            #Crop supplied object to the spatial polygon and then mask
            #b.crop <- crop(x,m@poly.ROI)
            #b <- mask(r,sp@boundary)
            b <- vals

            #Get pixel area
            pxl.area <- area(b)

            #Calculate the terms in the weighted average
            temp.by.area <- b*pxl.area
            na.by.area   <- (!is.na(b))*pxl.area
            wt.temp <- cellStats(temp.by.area,sum)/cellStats(na.by.area,sum)

            return(data.frame(realization=1:nlayers(b),value=wt.temp))
          })


setMethod("returns.field",signature(st="spatial.mean"),
          function(st){
            return(FALSE)
          })


#' Pass-through statistic
#'
#' Returns the field that was supplied. This can be useful, for example, if we
#' want to look at anomaly correlation coefficients and just want to pass the 
#' value straight through into the statistics processing and skill metric system
#' @export pass.through
pass.through <- setClass("pass.through",contains="stat",
                         prototype=list(name="pass.through"))

#' @export
setMethod("eval.stat",signature(st="pass.through",vals="Raster"),
          function(st,vals,...) {
            return(tibble(field=list(vals)))
          })

setMethod("returns.field",signature(st="pass.through"),
          function(st){
            return(TRUE)
          })



#' Northward extent of an isolone
#'
#' Calculates the average latitudinal position extent of an isoline
#' @export isoline.lat
isoline.lat <- setClass("isoline.lat",slots=list(threshold="numeric"),
                            contains="stat",
                            prototype=list(name="isoline.lat"))

#' @export
setMethod("eval.stat",signature(st="isoline.lat",vals="Raster"),
          function(st,vals,...){

            require(reshape2)

            #Crop supplied object to the spatial polygon and then mask
            # b.crop <- crop(x,m@poly.ROI)
            # b <- mask(b.crop,m@poly.ROI)
            b <- vals

            #Calculate the zonal averages - this has to be done
            #by hand, as there is no direct support
            zonal.mean <- raster::rowSums(b,na.rm=TRUE)/raster::rowSums(!is.na(b))

            #Loop over temperature thresholds
            lat.val.l <- list()
            for(i in seq(length(m@threshold))) {
              lat.val <- apply(as.matrix(zonal.mean),2,
                                      function(zm) {
                                        res <- try(approx(zm,yFromRow(b),
                                                      m@threshold[i],
                                                      rule=2,ties=min)$y)
                                        if(is(res,"try-error")) {res <- NA}

                                        return(res) })
              lat.val.l[[i]] <- data.frame(realization=getZ(b),
                                           threshold=m@threshold[i],
                                           value=lat.val)
            }
            lat.vals<- bind_rows(lat.val.l)

            return(lat.vals)
          })

setMethod("returns.field",signature(st="isoline.lat"),
          function(st){
            return(FALSE)
          })


#' Habitat suitability model 
#'
#' Applies a habitat suitability model
#' @export habitat.suitability
habitat.suitability <- setClass("habitat.suitability",
                                slots=list(model="function"),
                                prototype=list(name="habitat.suitability"),
                                contains="stat")

#' @export
setMethod("eval.stat",signature(st="habitat.suitability",vals="Raster"),
          function(st,vals,...){
            
            require(dplyr)
            
            #Apply the habitat model
            hab.r <- vals
            hab.r[] <- st@model(vals[])
            
            #Get pixel area
            pxl.area <- area(hab.r)
            
            #Calculate total carrying capacity
            pxl.cap <- pxl.area*exp(hab.r)
            car.cap <- cellStats(pxl.cap,sum,na.rm=TRUE)
            return(data.frame(realization=1:nlayers(vals),value=car.cap)) })


setMethod("returns.field",signature(st="habitat.suitability"),
          function(st){
            return(FALSE)
          })

