#/*======================================================================*/
#  Define common directories etc
#/*======================================================================*/
PE.cfg <- list()
PE.cfg$VOI.name <- "Variable_of_interest"
PE.cfg$ROI.extraction.buffer <- 2  #Degrees
PE.cfg$n.CMIP.chunks <- 5  #How many chunks should the CMIP5 ensemble be broken into?
PE.cfg$NMME.epoch.start <- as.Date("1960-01-01")

#Directories
PE.cfg$dirs <- list(Misc.meta="Z.Misc.meta",
                    datasrc="data_srcs",
                    cfg="scratch/Job_configuration")

#File names
PE.cfg$files <- list(Obs.monthly.anom.metadata=file.path(PE.cfg$dir$Misc.meta,"Monthly_anom_metadata.RData"),
                     Obs.climatology.metadata=file.path(PE.cfg$dir$Misc.meta,"Climatology_metadata.RData"),
                     fragment.meta=file.path(PE.cfg$dir$Misc.meta,"Fragment_metadata.RData"),
                     fragstack.meta=file.path(PE.cfg$dir$Misc.meta,"Fragstack_metadata.RData"),
                     anom.meta="Anomaly_metadata.RData",
                     realmean.meta="Realmean_metadata.RData",
                     analysis.grid="analysis.grid",
                     remapping.wts="remapping_wts.nc",
                     ensmean.name="Ensmean",
                     regridded.landmask="Regridded_landmask.nc")

usethis::use_data(PE.cfg,overwrite = TRUE)