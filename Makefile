#----------------------------------------------------------------------
# Predictability Engine HPC Processing Makefile
# 
# Mark Payne
# DTU-Aqua
# http://www.staff.dtu.dk/mpay
# Tue Aug  7 09:53:38 2018
#
# Makefile for keeping track of which runs have been performed and
# were successful. Note that the Makefile does not do any processing
# but instead just distributes the work to the cluster queueing system
# via qsub
#
# Note also that the useage of this is a bit non-standard - to build
# a specific type of file, you have to specify the TYPE argument e.g.
#   make TYPE=NMME
#
# A list of available types, and their current status can be obtained 
# using 
#   make status
#
# Available targets
#    help      ; Displays Makefile header with list of target
#    clean     : Remove all record files
# ----------------------------------------------------------------------
  
#Sources, variables
CFG_DIR=./scratch/Job_configuration
TODO_DIR=$(CFG_DIR)/TODO
CFGS=$(wildcard $(CFG_DIR)/*.cfg)
TODOs=$(wildcard $(TODO_DIR)/*.todo)
TYPES=$(notdir $(basename $(CFGS)))
TYPE_DIRS=$(addprefix $(CFG_DIR)/,$(TYPES))

OUTDIR=$(CFG_DIR)/$(TYPE)
JOB_LIST=$(shell grep -o "^[0-9]\+" $(CFG_DIR)/$(TYPE).cfg)
OKs=$(addprefix $(OUTDIR)/, $(addsuffix .ok, $(JOB_LIST)))
TODO=$(TODO_DIR)/$(TYPE).todo

#Target file lists
MASTER=./src/Y.HPC_scripts/qMaster.sh

#Default target
default: help status

#-------------------------------------
#NMME
THIS:  todo $(OKs)
	TASK_IDS=`paste -s -d "," $(TODO)`; qsub -N PE_$(TYPE) -v NAME=$(TYPE) -t $$TASK_IDS $(MASTER)
	
$(OUTDIR)/%.ok: 
	@echo $* >> $(TODO)
	
#-------------------------------------
#Remove any existing To do files
todo: $(TODOs)

$(TODO_DIR)/%.todo: FORCE 
	cat /dev/null  > $@ 

status: $(addsuffix .status,$(TYPES))

%.status: $(CFG_DIR)/%.cfg
	@echo `printf "%-30s" "$*"`  : `ls $(CFG_DIR)/$*/ | wc -l` out of `grep -c "^[0-9]\+," $<` jobs

setup: FORCE
	-mkdir $(TYPE_DIRS)
	-mkdir $(TODO_DIR)

#-------------------------------------
clean:
	-rm $(TODO_DIR)/*

FORCE:

#Targets
help:
	@#Display help up to first empty line
	@grep "^ *$$" Makefile -n -m1 |   sed "s/\(^.*\):.*/\1/" | xargs -I {} head -n{} Makefile


