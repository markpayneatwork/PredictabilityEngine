=========================
Blue Fin Tuna Predictability v2
=========================

Mark R. Payne
DTU-Aqua, Charlottenlund, DK
http://www.staff.dtu.dk/mpay

Last updated : Thu Apr 14 17:57:39 CEST 2016

Description
===========
This project develops predictions of Blue Fin Tuna distribution based on analysis both the MPI-OM model, and the NMME ensemble. Note that this represents a complete rewrite of v1, which was largely based on R and the raster package - here we use the climate data operators CDO and NCO whereever possible, due to their superior speed and functionality.

Data Sources
============
landmask.nc - obtained directly from the NMME archive

Setup
=====
* The PredEng package should be installed. If not installed in advance, it should be available (e.g. via a softlink) in the ./resources/PredEng_Package directory so that it can be installed using ./src/A1.Install_support_tools.r
* The ./scratch directory should be configured with lots of space available to serve as storage for intermediate files. This can be, for example, on the HPC scratch.

Workflow (e.g. for updating time series)
=======================================
The basic approach applied here is to take a whole lot of disparate datasets from different sources and bring them together in a common format. The core of this is the fragment, which is a 2D NetCDF field (lon-lat) for a given forecast date, start date, lead time, CMIP experiment etc. These are then recombined into:
* A.anom - 3D anomaly files (lat-lon-realization) 
* B.realmean - 2D realization mean fields
* C.ensmean - 2D ensemble mean fields 
Downstream scripts should only access these directories, which are constant between datasources (as appropriate), rather than the intermediate directories.

In addition, each of these data types should be accompanied by a metadata file, describing the nature of the files available. The contents of the metadata files have preference and direct the appropriate script to the appropriate individual fragment files. All metadata tables should contain the following headers (as a minimum):
* name - name of the PredEng.source object
* type - the type of data. For CMIP5 variables, includes the expt e.g. CMIP5.rcp85
* date - of the forecast/observation/projection, not of the model initialisation
* start.date - when the forecast is initialisation
* n.realizations - the number of realizations stored in the fragment / fragstack
* fname - including the full path relative to the project directory



