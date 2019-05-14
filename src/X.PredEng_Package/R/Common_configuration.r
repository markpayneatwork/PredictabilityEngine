#/*======================================================================*/
#  Define common directories etc
#/*======================================================================*/
PE.cfg <- list()
PE.cfg$VOI.name <- "Variable_of_interest"
PE.cfg$ROI.extraction.buffer <- 2  #Degrees
PE.cfg$n.CMIP.chunks <- 5  #How many chunks should the CMIP5 ensemble be broken into?
PE.cfg$NMME.epoch.start <- as.Date("1960-01-01")

#Configuration
PE.cfg$config.fname <- "configuration.rds"
PE.cfg$config.path <- file.path("objects",PE.cfg$config.fname)

#Directories
PE.cfg$dirs <- list(Misc.meta="Z.Misc.meta",
                    statistics="Statistics",
                    collated.stats="Collated.Stats",
                    datasrc="data_srcs",
                    job.cfg="scratch/Job_configuration")

#File names
PE.cfg$files <- list(Obs.monthly.anom.metadata=file.path(PE.cfg$dir$Misc.meta,"Monthly_anom_metadata.rds"),
                     Obs.climatology.metadata=file.path(PE.cfg$dir$Misc.meta,"Climatology_metadata.rds"),
                     src.meta="Src_metadata.rds",
                     fragment.meta=file.path(PE.cfg$dir$Misc.meta,"Fragment_metadata.rds"),
                     fragstack.meta=file.path(PE.cfg$dir$Misc.meta,"Fragstack_metadata.rds"),
                     clim.meta="Climatology_metadata.rds",
                     anom.meta="Anomaly_metadata.rds",
                     realmean.meta="Realmean_metadata.rds",
                     stats="All_stats.rds",
                     skill.metrics="Skill_metrics.rds",
                     analysis.grid="analysis.grid",
                     remapping.wts="remapping_wts.nc",
                     ensmean.name="Ensmean",
                     regridded.landmask="Regridded_landmask.nc")

usethis::use_data(PE.cfg,overwrite = TRUE)