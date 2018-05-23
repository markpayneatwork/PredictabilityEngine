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
The basic approach applied here is to take a whole lot of disparate datasets from different sources and bring them together in a common format, expressed in terms of anomalies. 

