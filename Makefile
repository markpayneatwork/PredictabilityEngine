#----------------------------------------------------------------------
# Predictability Engine HPC Processing Makefile
# 
# Mark Payne
# DTU-Aqua
# http://www.staff.dtu.dk/mpay
# Tue Aug 25 15:53:23 CEST 2020
#
# Makefile for doing the absolute basic functions in running the
# Predictability Engine. Most of the make-like functionality is now
# controlled from R, via the drake package
#
# A list of available types, and their current status can be obtained 
# using 
#   make status
#
# Additional targets
#    help      ; Displays Makefile header with list of target
#    clean     : Remove all log files from working directory
#    install   : Installs support R packages
#
#    NMME_sync : Downloads NMME data via OpenDAP
#
#    make      : Submit make script to cluster
#    status    : Get status of make
#
# ----------------------------------------------------------------------

#Setup
Rscript= Rscript #/appl/R/bin/Rscript-3.5.2-sl73
  
#Default target
default: help 

#-------------------------------------
#Remove any existing To do files

make: FORCE
	bsub < src/Y.HPC/DrakeMake.sh

install: FORCE
	$(Rscript) src/ZZ.Helpers/Setup_system.r

NMME_sync:
	bsub  < src/Y.HPC_scripts/bNMME_sync.sh

#-------------------------------------
clean:
	-@rm PE_* -f
	-@rm NMME_* -f

FORCE:

#Targets
help:
	@#Display help up to first empty line
	@grep "^ *$$" Makefile -n -m1 |   sed "s/\(^.*\):.*/\1/" | xargs -I {} head -n{} Makefile


