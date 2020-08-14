#/*======================================================================*/
#  Define common directories etc
#/*======================================================================*/
.onLoad <- function(libname, pkgname) {
  PE.cfg <- list()

  #Directories
  PE.cfg$PE.dir.objects <- file.path("objects")
  PE.cfg$PE.dir.datasrc <- file.path("data_srcs")

  #Configuration
  PE.cfg$PE.path.config   <- file.path(PE.cfg$PE.dir.objects,"configuration.rds")
  PE.cfg$PE.path.datasrcs <- file.path(PE.cfg$PE.dir.objects,"datasrcs.RData")
  
  # PE.cfg$PE.dirs <- list(Misc.meta="Z.Misc.meta",
  #                     statistics="Statistics",
  #                     results="Results",
  #                     datasrc="data_srcs",
  #                     job.cfg="scratch/Job_configuration")
  
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
  
  #Set options
  options(PE.cfg)
  
  return(invisible())
}

#' Get Standard Filenames
#'
#' @param pcfg PE.cfg object
#' @param type Type of filename to retrieve
#'
#' @return The path to the particular file
#' @export
#'
PE.fname <- function(pcfg,type) {
  scratch.fnames <- list(analysis.grid="analysis.grid",
                         landmask="landmask.nc",
                         config=basename(getOption("PE.path.config")))
  rtn <- file.path(pcfg@scratch.dir,scratch.fnames[[type]])
  return(rtn)
}