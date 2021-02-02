#' Display configuration status
#'
#' @param ... Relevant paramters to be displayed
#'
#' @export
PE.config.summary<- function(...){
  
  l <- list(...)
  for(i in seq(l)){
    this <- l[[i]]
    this.name <- names(l)[i]
    if(is(this,"PredEng.config")) {
      show(this)
      log_msg("---------------------\n")}
    if(is(this,"data.source")) {
      log_msg("Data source          : %s (%s) \n",this@name,this@type)}
    if(is(this,"spatial.domain"))     {
      log_msg("Spatial domain       : %s (%s)\n",this@name,this@desc)}
    if(is.numeric(this)) {
      log_msg("%-20s : %i\n",this.name,this)
    }
    if(is.character(this)) {
      log_msg("%-20s : %s\n",this.name,this)
    }
  }
  log_msg("---------------------\n")
  log_msg("%-20s : %s\n","R.version", R.version$version.string)
  log_msg("%-20s : %s\n","Git branch",system2("git","branch --show-current",stdout = TRUE))
  log_msg("%-20s : %s\n","Git commit",system2("git","log --pretty=format:'%h' -n 1",stdout = TRUE))
  log_msg("%-20s : %s\n","Git comment",
          paste(system2("git","log --pretty='%B' -n 1",stdout = TRUE),collapse=" "))
  log_msg("%-20s : %s\n","Commit date",system2("git","log --pretty=format:'%cd' -n 1",stdout = TRUE))
  log_msg("---------------------\n")
}


#' Create sf polygon from raster extent object
#'
#' @param ext raster extent object
#'
#' @return sf POLYGON object
#' @export
sfpolygon.from.extent <- function(ext) {
  ext %>% 
    coordinates() %>%
    rbind(.,head(.,n=1)) %>%  #Make it loop around
    list() %>%
    st_polygon() 
}


#' Get a global ROI as simple features object
#'
#' @param this.pcfg PCFG, containing a valid global.ROI object
#'
#' @return
#' @export
#'
PE.global.sf <- function(this.pcfg) {
  st_sf(geometry=st_sfc(sfpolygon.from.extent(this.pcfg@global.ROI)),
        name=PE.cfg$misc$globalROI,
        crs=crs(this.pcfg@spatial.polygons))
        
}

#' Month handling functions 
#' 
#' Works with dates in terms of their months only, ignoring days. Handy for standardising time where there is
#' a risk that the days of the month may be different between data sources.
#'
#' @param d Vector of dates to convert, as Date classes
#' @param t1 First Date
#' @param t2 Second Date
#'
#' @return String of format yyyy-mm
#' @export
#' @name month_fns
date_to_ym <- function(d) {
  sprintf("%i-%02i",year(d),month(d))
}

#' @export
#' @rdname month_fns
month_diff <- function(t1,t2) {
  year(t1)*12+month(t1) - year(t2)*12-month(t2)
}


