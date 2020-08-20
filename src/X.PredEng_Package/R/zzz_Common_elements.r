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

