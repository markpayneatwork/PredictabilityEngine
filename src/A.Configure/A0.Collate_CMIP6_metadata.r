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
CMIP6.db <- 
  tibble(path=dir(here(PE.cfg$dir$datasrc,"CMIP6"),
                  recursive = TRUE,pattern="*.nc$",full.names = TRUE)) %>%
  mutate(file.size=file.size(path),
         fname=basename(path)) %>%
  separate(fname,sep="_",
           into=c("variable","table","source","experiment","member","grid","time_range")) %>%
  #Remove empty files
  filter(file.size!=0) %>%
  #Get grid info and discard contents asap
  mutate(two.var=map2(path,variable,~list(path=.x,variable=.y)),
         dim.meta=pblapply(two.var,cl=8,
                        FUN=function(this) {
                          ncid <- nc_open(this$path)
                          rtn <- 
                            tibble(dim=ncid$var[[this$variable]]$dim) %>% 
                            mutate(atts=map(dim,~ncatt_get(ncid,.x$name))) %>%
                            hoist(atts,"axis","units","standard_name","long_name","calendar") %>% 
                            hoist(dim,dimname="name",length="len",values="val") %>% 
                            select(-dim,-atts)
                          nc_close(ncid)
                          return(rtn)
                        })) %>% 
  select(-two.var)

#Extract grid and axis metadata
axis.meta <- 
  CMIP6.db %>% 
  mutate(max.xy.dim=map_dbl(dim.meta,~ .x %>% filter(!(axis %in% c("Z","T"))) %>% pull(length) %>% max()),
         map_dfr(dim.meta,
                 function(.x) {
                   .x <- mutate(.x,
                                name=ifelse(is.na(standard_name),long_name,standard_name))
                   if("Z" %in% .x$axis | "Vertical T levels" %in% .x$long_name) {
                     rtn <- .x %>% filter(axis=="Z")  
                     if(nrow(rtn)==0){ 
                       rtn <- 
                         .x %>% 
                         filter(is.na(axis),"Vertical T levels"==long_name) }
                   }  else {
                     rtn <- tibble(name=NA_character_,units=NA_character_)
                   }
                   return(select(rtn,zaxis.name=name,zaxis.units=units))}))

axis.meta %>% 
  select(-dim.meta) %>%
  saveRDS(file=PE.cfg$path$CMIP.metadata)

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

