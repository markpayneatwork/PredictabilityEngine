PE.cfg <- list()

#Directories
PE.cfg$dir <- list(objects="objects",
                    datasrc= "data_srcs",
                   scratch="scratch")

#Configuration
PE.cfg$path$config   <- file.path(PE.cfg$dir$objects,"configuration.rds")
PE.cfg$path$datasrcs <- file.path(PE.cfg$dir$objects,"datasrcs.RData")

#Filenames
PE.cfg$file <- list(analysis.grid="analysis.grid",
                    landmask="landmask.nc",
                    config=basename(PE.cfg$path$config),
                    statjoblist="statjoblist.rds",
                    logs="logs/")

#Results database tables
PE.cfg$db <- list(extract="extraction",
                  climatology="climatology",
                  calibration="calibration",
                  stats="statistics")

#Validity
PE.cfg$validity <- list(calibrationMethods=c("anomaly","Mean adjusted"))

# PE.cfg$PE.dirs <- list(Misc.meta="Z.Misc.meta",
#                     statistics="Statistics",
#                     results="Results",

#Misc
PE.cfg$misc <- list(crs=CRS("+proj=longlat +datum=WGS84"))  #The analysis grid currently only
                                                            #cable of working on lon-lat

#File names
# PE.cfg$PE.files <- list(Obs.monthly.anom.metadata=file.path(PE.cfg$PE.dir$Misc.meta,"Monthly_anom_metadata.rds"),
#                      Obs.climatology.metadata=file.path(PE.cfg$PE.dir$Misc.meta,"Climatology_metadata.rds"),
#                      stats.configuration=file.path(PE.cfg$PE.dirs$statistics,"Stats_configuration.rds"),
#                      src.meta="Src_metadata.rds",
#                      fragment.meta=file.path(PE.cfg$PE.dir$Misc.meta,"Fragment_metadata.rds"),
#                      fragstack.meta=file.path(PE.cfg$PE.dir$Misc.meta,"Fragstack_metadata.rds"),
#                      clim.meta="Climatology_metadata.rds",
#                      anom.meta="Anomaly_metadata.rds",
#                      realmean.meta="Realmean_metadata.rds",
#                      scalar.stats="Scalar_stats.rds",
#                      scalar.skill.metrics="Scalar_skill_metrics.rds",
#                      remapping.wts="remapping_wts.nc",
#                      ensmean.name="Ensmean")
# 
#Misc
# PE.cfg$PE.misc <- list(global.sp.name = "Global",
#                     VOI.name= "Variable_of_interest",
#                     NMME.epoch.start = as.Date("1960-01-01"))


usethis::use_data(PE.cfg,overwrite = TRUE)

#' Get Standard Filenames 
#'
#' @param pcfg PE.cfg object
#' @param type Type of filename  to retrieve
#'
#' @return The path to the particular file
#' @name filenames
#' @export
#'
PE.scratch.path <- function(pcfg,type) {
  scratch.types <- c(PE.cfg$file)
  rtn <- file.path(pcfg@scratch.dir,scratch.types[[type]])
  return(rtn)
}




