#/*##########################################################################*/
#' Define Data Sources
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' Thu Sep  1 19:17:39 2016
#'
#' Defines a set of commonly employed data sources 
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

#'========================================================================
# Initialise system
#'========================================================================
cat(sprintf("\n%s\n","Common Data Sources Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Source the common elements
suppressPackageStartupMessages({
  library(PredEng)
  library(pbapply)
})

#'========================================================================
# CMIP6 ####
#'========================================================================
log_msg("CMIP6...\n")

#Setup CMIP6 database of filenames first
#File naming convention, from https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit
#<variable_id>_<table_id>_<source_id>_<experiment_id >_<member_id>_<grid_label>[_<time_range>].nc
CMIP6.db.all <- 
  tibble(path=dir(here(PE.cfg$dir$datasrc,"CMIP6"),
                  recursive = TRUE,pattern="*.nc$",full.names = TRUE)) %>%
  mutate(file.size=file.size(path),
         fname=basename(path)) %>%
  separate(fname,sep="_",
           into=c("variable","table","source","experiment","member","grid","time_range")) %>%
  #Remove empty files
  filter(file.size!=0) %>%
  #Get grid info
  mutate(zaxisdes=pblapply(path,cl=8,
                           function(x) {
                             rtn <- cdo("-s -W zaxisdes",x)
                             if(length(rtn)==0) {
                               return(NA) 
                             }else {
                               return(rtn)
                             }}),
         zaxistype=map(zaxisdes,~gsub("^.*= ","",grep("zaxistype =",.x,value=TRUE))))


stop()

#Work through this systematically to first choose the right axis, then 
  CMIP6.db.all %>%
    mutate(n.zaxis=map_int(zaxistype,length)) %>%
    filter(n.zaxis>1)
    count(variable,n.zaxis)

#Print some summary data
CMIP6.db.all %>%
  count(variable,source,grid) %>%
  pivot_wider(names_from=c(grid),values_from=n) %>%
  print(n=Inf)

saveRDS(CMIP6.db.all,file=PE.cfg$path$CMIP.metadata)

#Turn off thte lights
log_msg("\nConfiguration complete.\n")

#' -----------
#' <small>*This work by Mark R Payne is licensed under a  Creative Commons
#' Attribution-NonCommercial-ShareAlike 3.0 Unported License.
#' For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
#' Basically, this means that you are free to "share" and "remix" for
#' non-commerical purposes as you see fit, so long as you "attribute" me for my
#' contribution. Derivatives can be distributed under the same or
#' similar license.*</small>
#'
#' <small>*This work comes with ABSOLUTELY NO WARRANTY or support.*</small>
#'
#' <small>*This work should also be considered as BEER-WARE. For details, see
#' http://en.wikipedia.org/wiki/Beerware*</small>
#'
#' -----------
#
# Fin

