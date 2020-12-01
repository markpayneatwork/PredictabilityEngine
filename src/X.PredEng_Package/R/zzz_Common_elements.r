#' Display configuration status
#'
#' @param ... Relevant paramters to be displayed
#'
#' @export
config.summary<- function(...){
  
  l <- list(...)
  for(this in l){
    if(is(this,"PredEng.config")) {
      show(this)
      log_msg("---------------------\n")}
    if(is(this,"data.source")) {
      log_msg("Data source          : %s (%s) \n",this@name,this@type)}
    if(is(this,"spatial.domain"))     {
      log_msg("Spatial domain       : %s (%s)\n",this@name,this@desc)}
    if(is.numeric(this)) {
      log_msg("Configuration id     : %i\n",this)
    }
  }
  log_msg("R.version            : %s\n",R.version$version.string)
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
