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
* The PredEng package should be available (e.g. via a softlink) in the ./resources/PredEng_Package directory

Workflow (e.g. for updating time series)
=======================================

