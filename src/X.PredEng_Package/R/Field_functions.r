#' Raster List functions
#' 
#' Helper functions to work with lists of rasters
#'
#' @param l List of raster layers
#'
#' @return
#' @name rasterList
#' @export
raster.list.mean <- function(l) {
  l.stack <- raster::brick(l)
  l.mean <- raster::mean(l.stack) #Dispatching can be a bit strange here sometimes
  return(list(l.mean))
}


#' @export
#' @rdname rasterList
raster.list.sd <- function(l) {
  l.stack <- raster::brick(l)
  l.sd <- raster::calc(l.stack,sd) #Dispatching can be a bit strange here sometimes
  return(list(l.sd))
}

