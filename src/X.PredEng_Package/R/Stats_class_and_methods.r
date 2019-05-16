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

#' Thresholds
#'
#' Determines where each pixel sits in relation to a threshold value, potentially integrating over the
#' area of interest
#' @inherit stat params
#' @param threshold Critical threshold value - a numeric of length 1
#' @param above Logical value - TRUE indicates that we wish to test for values above the threshold. FALSE below.
#' @param integrate Logical value - calculate the integrated area?
#' @export threshold

#' @return  If integrate==TRUE, then a tibble is returned corresponding to the integrated area above (or below) the threshold value. 
#' If integrate==FALSE, a tibble containing the Raster* object matching the raster object supplied as an
#' argument
threshold <- setClass("threshold",
                                 slots=list(threshold="numeric",
                                            above="logical",
                                            integrate="logical"),
                                 prototype=list(name="threshold",
                                                above=TRUE,
                                                integrate=FALSE),
                                 contains="stat",
                                 validity = function(object) {
                                   err.msg <- NULL
                                   if(length(object@threshold)!=1) {
                                     err.msg <- c(err.msg,
                                                  sprintf("Length of 'threshold' slot should be 1 but is %i.",length(object@threshold)))
                                   }
                                   if(length(err.msg)==0) return(TRUE) else err.msg
                                 })

#' @export
setMethod("eval.stat",signature(st="threshold",vals="Raster"),
          function(st,vals,...){
            if(st@above) {
              pass.threshold <- vals > st@threshold
            } else {
              pass.threshold <- vals < st@threshold
            }
            
            if(st@integrate) {
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
              return(tibble(field=list(res)))
            }

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


