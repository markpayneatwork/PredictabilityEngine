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
# Available targets
#    NMME      : Processes NMME models
# 
#    help      ; Displays Makefile header with list of target
#    vars      : Displays some useful variables
#    clean     : Remove all record files
# ----------------------------------------------------------------------
  
#Sources, variables
CFG_DIR=./scratch/Job_configuration
NMME_CFGS=$(shell grep -o "^[0-9]\+" $(CFG_DIR)/NMME.cfg)
NMME_OUTDIR=$(CFG_DIR)/NMME
NMME_OKs=$(addprefix $(NMME_OUTDIR)/, $(addsuffix .ok, $(NMME_CFGS)))
NMME_TODO=$(TODO_DIR)/NMME.todo

#Target file lists
MASTER=./src/Y.HPC_scripts/qMaster.sh
TODO_DIR=$(CFG_DIR)/TODO
TODOs=$(wildcard $(TODO_DIR)/*.todo)

#Default target
default: help


#-------------------------------------
#NMME
NMME:  todo $(NMME_OKs)
	TASK_IDS=`paste -s -d "," $(NMME_TODO)`; qsub -N PE_NMME -v NAME=NMME -t $$TASK_IDS $(MASTER)
	
$(NMME_OUTDIR)/%.ok: 
	@echo $* >> $(NMME_TODO)
	
#-------------------------------------
#Remove any existing To do files
todo: $(TODOs)

$(TODO_DIR)/%.todo: FORCE 
	cat /dev/null  > $@ 

setup: FORCE
	mkdir $(NMME_OUTDIR)
	mkdir $(TODO_DIR)

#-------------------------------------
clean:
	-rm $(TODO_DIR)/*

FORCE:

#Targets
help:
	@#Display help up to first empty line
	@grep "^ *$$" Makefile -n -m1 |   sed "s/\(^.*\):.*/\1/" | xargs -I {} head -n{} Makefile


