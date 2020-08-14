Predictability Engine
=========================

Mark R. Payne
Technical University of Denmark (DTU-Aqua)
Kgs. Lyngby, DK
http://www.staff.dtu.dk/mpay

Description
===========
This project provides a general interface to working with seasonal and decadal climate predictions, climate projections, and associated observational data. The engine handles extraction, calibration, application of impact models, and verification. 

Setup
=====
* Associated packages can be installed using ./src/ZZ.Helpers/Setup_system.r
* The engine also requires the availability of CDO and NCO as well
* The ./scratch directory should be configured with lots of space available to serve as storage for intermediate files. This can be, for example, on the HPC scratch.

Workflow
========
The workflow for this engine has been reinvented multiple times. The basic approach applied here is to take a whole lot of disparate datasets from different sources and bring them together in a common format. The sticking point has always been when and how this should be done in the most efficient and flexible manner - solutions have not always been user friendly though. The basic workflow can be divided up into five steps

A. Configure
B. Extract data
C. Calibrate data
D. Apply impact models
E. Calculate verification statistics

Specifications for the extraction are set in Step A, using a custom configuration class.

The core of thie data analysis is the grid-fragment, which is a 2D NetCDF field (lon-lat) for a given forecast date, start date, lead time, CMIP experiment etc. Each fragment is stored as a raster object in a tibble, with associated metadata. All metadata tables should contain the following headers (as a minimum):
* name - name of the PredEng.source object
* type - the type of data. For CMIP5 variables, includes the expt e.g. CMIP5.rcp85
* date - of the forecast/observation/projection, not of the model initialisation
* start.date - when the forecast is initialisation
* realization - the identifier of the realization. This can also be "realmean", meaning that it is the average across realisations. 

Fragments are primarily generated by step B above and saved as .rds files, split into data sources.

Step C generates secondary .rds files derived from the primary Step B files. There can be multiple calibration methods employed, resulting in multiple targets for downstream impact models.

Steps D and E build from there.
