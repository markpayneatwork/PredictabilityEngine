PE.cfg <- list()

#Directories
PE.cfg$dir <- list(objects="objects",
                    datasrc= "data_srcs",
                   scratch="scratch")

#Configuration
PE.cfg$path$config   <- file.path(PE.cfg$dir$objects,"configuration.rds")
PE.cfg$path$datasrcs <- file.path(PE.cfg$dir$objects,"datasrcs.rds")
PE.cfg$path$NAOmatching <- file.path(PE.cfg$dir$objects,"NAOmatching.rds")
PE.cfg$path$CMIP.metadata <- file.path(PE.cfg$dir$objects,"CMIP.metadata.rds")

#Filenames
PE.cfg$file <- list(analysis.grid="analysis.grid",
                    landmask="landmask.rds",
                    landmask.pdf="landmask.pdf",
                    config=basename(PE.cfg$path$config),
                    statjoblist="statjoblist.rds",
                    logs="logs/")

#Results database tables
PE.cfg$db <- list(extract="Extraction",
                  climatology="Climatology",
                  calibration="Calibration",
                  stats="Statistics",
                  metrics="Metrics",
                  metrics.field="MetricsField",
                  pt.extraction="PtExtraction")

#Validity
PE.cfg$validity <- list(calibrationMethods=c("None","anomaly","MeanAdj","MeanVarAdj","NAOmatching"))

# PE.cfg$PE.dirs <- list(Misc.meta="Z.Misc.meta",
#                     statistics="Statistics",
#                     results="Results",

#Misc
PE.cfg$misc <- list(globalROI="global.ROI",
                    crs=CRS("+proj=longlat +datum=WGS84"))  #The analysis grid currently only
                                                            #capable of working on lon-lat


#Store versions that this package is built with
PE.cfg$version <- 
  list(branch=system2("git","branch --show-current",stdout = TRUE),
       commit=system2("git","log --pretty=format:'%h' -n 1",stdout = TRUE),
       comment=paste(system2("git","log --pretty='%B' -n 1",stdout = TRUE),collapse=" "),
       date=system2("git","log --pretty=format:'%cd' -n 1",stdout = TRUE))

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



