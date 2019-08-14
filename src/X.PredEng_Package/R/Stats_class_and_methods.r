# Base stats class ========================================================================

#' Statistics class
#'
#' @param name Name of the statistics class
#' @param use.realmeans Indicates whether to use the mean of the individ realisations or
#' the realisation values themselves. This slot essentially acts as a flag telling
#' the script whether it wants 2D (lat-lon) or 3D (lat-lon-realization) data.
#' @param use.full.field Should the statistic be calculated on the basis of
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
                                use.full.field="logical",
                                is.global.stat="logical",
                                skill.metrics="character"),
                     prototype = list(use.realmeans=TRUE,
                                      use.full.field=TRUE,
                                      is.global.stat=FALSE))


#' Evaluate an stat
#' @export
setGeneric("eval.stat",
           function(st,dat)
             standardGeneric("eval.stat")
)

#' Return type
#' @export
setGeneric("returns.field",
           function(st)
             standardGeneric("returns.field"))

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

            #Now calculate the area
            pxl.area <- area(dat)
            area.masked <- pxl.area * pass.threshold
            names(area.masked) <- names(dat)
            area.satistfying.thresh <- cellStats(area.masked,sum)
            
            #Filter areas where it doesn't work.
            mean.temp <- cellStats(dat,mean)
            area.filt <- ifelse(is.na(mean.temp),NA,area.satistfying.thresh)
            
            #Return
            thresh.layers <- purrr::map(1:nlayers(pass.threshold),function(i) pass.threshold[[i]])
            return(tibble(field=thresh.layers,realization=1:nlayers(dat),value=area.filt)) 

          })

#' @export
setMethod("returns.field",signature(st="threshold"),
          function(st){
            return(TRUE)
          })



# Averaging ========================================================================


#' Average value within an ROI
#'
#' Calculates the average value of a variable within a region of interest using
#' area-weighting
#' @export spatial.mean
spatial.mean <- setClass("spatial.mean",contains="stat",
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

            return(tibble(realization=1:nlayers(dat),value=wt.temp))
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
pass.through <- setClass("pass.through",contains="stat",
                         prototype=list(name="pass.through"))

#' @export
setMethod("eval.stat",signature(st="pass.through",dat="Raster"),
          function(st,dat) {
            return(tibble(field=list(dat)))
          })

setMethod("returns.field",signature(st="pass.through"),
          function(st){
            return(TRUE)
          })

# Isoline ========================================================================


#' Northward extent of an isolone
#'
#' Calculates the average latitudinal position extent of an isoline
#' @export isoline.lat
isoline.lat <- setClass("isoline.lat",slots=list(threshold="numeric"),
                            contains="stat",
                            prototype=list(name="isoline.lat"))

#' @export
setMethod("eval.stat",signature(st="isoline.lat",dat="Raster"),
          function(st,dat){
            #Crop supplied object to the spatial polygon and then mask
            # b.crop <- crop(x,m@poly.ROI)
            # b <- mask(b.crop,m@poly.ROI)

            #Calculate the zonal averages - this has to be done
            #by hand, as there is no direct support
            zonal.mean <- raster::rowSums(dat,na.rm=TRUE)/raster::rowSums(!is.na(dat))

            #Loop over temperature thresholds
            lat.val.l <- list()
            for(i in seq(length(m@threshold))) {
              lat.val <- apply(as.matrix(zonal.mean),2,
                                      function(zm) {
                                        res <- try(approx(zm,yFromRow(dat),
                                                      m@threshold[i],
                                                      rule=2,ties=min)$y)
                                        if(is(res,"try-error")) {res <- NA}

                                        return(res) })
              lat.val.l[[i]] <- tibble(realization=getZ(dat),
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

# Habitat Model ========================================================================

#' Habitat model 
#'
#' Applies a habitat suitability model to calculate the habitat suitability on a pixel-by-pixel
#' basis and then integrates it
#' @name habitat
#' @export habitat
habitat <- setClass("habitat",
                    slots=list(fn="function",
                               resources="list"),
                    prototype=list(name="habitat"),
                    contains="stat",
                    validity = function(object) {
                      err.msg <- NULL
                      if(!identical(names(formals(object@fn)),c("dat","resources"))) {
                        err.msg <- c(err.msg,
                                     'Function in fn slot must have exactly two arguments: "dat" and "resources"')
                      }
                      if(length(err.msg)==0) return(TRUE) else err.msg
                    })

setMethod("eval.stat",signature(st="habitat",dat="Raster"),
          function(st,dat){

            #Apply the habitat model
            hab.r <- st@fn(dat,st@resources)

            #Get pixel area
            pxl.area <- area(hab.r)
            
            #Calculate total carrying capacity over ROI
            pxl.cap <- pxl.area*hab.r
            car.cap <- cellStats(pxl.cap,sum,na.rm=TRUE)
            
            #Break the field into indvidual layers
            hab.layers <- map(1:nlayers(hab.r),function(i) hab.r[[i]])
            
            #Return
            return(tibble(field=hab.layers,realization=1:nlayers(dat),value=car.cap)) })


setMethod("returns.field",signature(st="habitat"),
          function(st){
            return(FALSE)
          })

