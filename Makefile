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
#    help         ; Displays Makefile header with list of target
#    clean        : Remove all log files from working directory
#    install      : Installs support R packages
#    %.r          : (Re-)runs the file %.r e.g. a linked configuration file, but also others
#    local        : Perform the local extraction steps of the pipeline
#    analysis     : Performs the full analysis (without submitting to a queue)
#    watch        : Watch tail of the most recent LSF log
#    less	  : View all of most recent LSF log using less
#    tail 	  : View end of most recent target log file
#    srcs         : Rebuild source database
#
#    NMME_sync    : Downloads NMME data via OpenDAP
#
#    itso         : Make it so, ensign! Submit make script to cluster
#    status       : Get status of make
#
# ----------------------------------------------------------------------

#Setup
Rscript= ./Rscript #/appl/R/4.0.2-mkl2020/bin/Rscript
  
#Default target
default: help 

#-------------------------------------
#Remove any existing To do files

itso: FORCE
	bsub < src/Y.HPC/Make.sh

install: FORCE
	$(Rscript) src/ZZ.Helpers/Setup_system.r

%.r: FORCE
	$(Rscript) $@

local: 
	$(Rscript) src/ZZ.Helpers/targets.r local.data 

analysis: 
	$(Rscript) src/ZZ.Helpers/targets.r analysis

NMME_sync:
	bsub  < src/Y.HPC_scripts/bNMME_sync.sh

srcs:
	$(Rscript) src/A.Configure/A1.Define_common_data_srcs.r

#-------------------------------------
clean:
	-@rm PE_* -f
	-@rm NMME_* -f

FORCE:

#No default suffixes
.SUFFIXES:   

#Targets
help:
	@#Display help up to first empty line
	@grep "^ *$$" Makefile -n -m1 |   sed "s/\(^.*\):.*/\1/" | xargs -I {} head -n{} Makefile
R:
	$(Rscript) -e "sessionInfo()"

watch:
	watch 'ls _targets/*.out | tail -n 1 | xargs tail -n 50'
less:
	ls _targets/*.out | tail -n 1 | xargs less
tail:
	watch 'ls $$PWD/_targets/logs/* -trd | tail -n1 |xargs tail -n 50'

