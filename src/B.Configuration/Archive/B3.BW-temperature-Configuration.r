#/*##########################################################################*/
#' Blue whiting temperature Configuration
#' ==========================================================================
#'
#' by Mark R Payne  
#' DTU-Aqua, Charlottenlund, Denmark  
#' http://www.staff.dtu.dk/mpay  
#'
#' TSun Sep  4 23:22:56 2016
#'
#' Configures a blue whiting temperature object
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

# ========================================================================
# Initialise system
# ========================================================================
cat(sprintf("\n%s\n","Blue whiting temperature Configuration"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();

#Source the common elements
library(PredEng)

# ========================================================================
# Configuration
# ========================================================================
#Global project configuration
pcfg <- config(name= "BW-Temp",
               res=0.5,
               MOI=c(3,4),  #August  
               clim.years=1961:2010,    #1982 excluded because not all leads present here
               comp.years=1961:2012,    
               landmask="data_srcs/NMME/landmask.nc")
               # observations=SST_obs[c("HadISST","EN4","OISST")],
               # hindcast.models=hindcast_mdls,
               # uninit.models=uninit_mdls)

#Define ROI
pcfg@ROI <- extent(-25,0,40,65)


#Time series bbox
pcfg@indicators <- list(new("area.wt.mean",
                            ROI=extent(-20,-5,50,60)))

#Misc
pcfg@analysis.grid <- file.path("processing",pcfg@name,"analysis.grid")

#Define observational sources
pcfg@observations <- list(data_src(name="EN4",type="obs",
                                     var="temperature",
                                     levels=17:23))

#Only one model to chose from here
pcfg@hindcast.models <- list(GCM(name="MPI-ESM-LR",var="thetao",
                                 type="hindcast",
                                 levels=13:19,
                                 ensmem_fn=CMIP5_ensmem,
                                 init_fn=function(f){
                                   init.str <- str_match(basename(f),"^.*?_([0-9]{6})-[0-9]{6}.*$")[,2]
                                   init.date <- ymd(paste(init.str,"01",sep=""))
                                   return(init.date)}))


#Update everything
pcfg <- update(pcfg)

# ========================================================================
# Output
# ========================================================================
dmp <- define_dir(file.path("processing",pcfg@name))

#Write CDO grid descriptor
writeLines(griddes(pcfg),pcfg@analysis.grid)

#Output
save.image(file="objects/configuration.RData")
save.image(file=file.path("processing",pcfg@name,"configuration.RData"))

# ========================================================================
# Done
# ========================================================================
#Turn off thte lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log_msg("\nAnalysis complete.\n")

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
