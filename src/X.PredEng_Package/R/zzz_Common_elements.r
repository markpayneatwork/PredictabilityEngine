#/*##########################################################################*/
#' Predictability Engine Common Elements
#' ==========================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' http://www.staff.dtu.dk/mpay
#'
#' Fri May 20 09:55:40 2016
#'
#' Defines a set of common-baseline elements for use across all pieces of code
#' in this codebase
#
#  This work is subject to a Creative Commons "Attribution" "ShareALike" License.
#  You are largely free to do what you like with it, so long as you "attribute"
#  me for my contribution. See the fine print at the end for exact details.
#
#  To do:
#
#  Notes:
# - While this script contains reminants of RMarkdown, it is not in a state
#    where it can be compiled in a meaningful manner
#/*##########################################################################*/

#Load packages
# library(raster)
# library(lubridate)
# library(stringr)

# ========================================================================
# Some constants
# ========================================================================
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

