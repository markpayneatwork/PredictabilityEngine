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
#'
#' @export stat
#' @exportClass stat
stat <- setClass("stat",
                     slots=list(name="character",
                                use.realmeans="logical",
                                use.anomalies="logical"),
                     prototype = list(use.realmeans=TRUE,
                                      use.anomalies=FALSE))


#' Evaluate an stat
#' @export
setGeneric("eval.stat",
           function(st,vals, ...)
             standardGeneric("eval.stat")
)

#' Area above a threshold
#'
#' Calculates the area of water above a threshold temperature
#' @param threshold Critical threshold value
#' @export area.above.threshold
area.above.threshold <- setClass("area.above.threshold",
                                 slots=list(threshold="numeric"),
                                 prototype=list(name="area.above.threshold"),
                           contains="stat")

#' @export
setMethod("eval.stat",signature(st="area.above.threshold",vals="Raster"),
          function(st,vals,...){

            require(dplyr)

            #Crop supplied object to the spatial polygon and then mask
            #b.crop <- crop(x,m@poly.ROI)
            #b <- mask(r,sp@boundary)
            b <- vals

            #Get pixel area
            pxl.area <- area(b)

            #Loop over temperature thresholds
            areas.l <- lapply(st@threshold,function(t) {
              #Calculate areas
              over.thresh <- b>t
              area.masked <- pxl.area * over.thresh
              names(area.masked) <- names(b)
              area.overthresh <- cellStats(area.masked,sum)
              #Filter areas where it doesn't work.
              mean.temp <- cellStats(b,mean)
              area.filt <- ifelse(is.na(mean.temp),NA,area.overthresh)
              #Return
              return(data.frame(realization=getZ(b),threshold=t,value=area.filt)) })

            #Tidy up output
            areas <- bind_rows(areas.l)
            return(areas)
          })


#' Average temperature within an ROI
#'
#' Calculates the average temperature within a region of interest using
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

            return(data.frame(realization=getZ(b),value=wt.temp))
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
            return(data.frame(realization=getZ(vals),value=car.cap)) })


