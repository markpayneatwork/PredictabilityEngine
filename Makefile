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
# a specific type of file, you have to specify the TYPE argument together
# with the "Cluster" target e.g.
#   make cluster TYPE=NMME
#
# A list of available types, and their current status can be obtained 
# using 
#   make status
#
# Available targets
#    help      ; Displays Makefile header with list of target
#    purge     : Remove all todo files
#    clean     : Remove all log files from working directory
#    status    : List of available types and their current status
#    setup     : Creates necessary subdirectories
#    install   : Installs various R packages
#    NMME_sync : Downloads NMME data via OpenDAP
#
#    persistence: Persistence forecast
#    collate   : Collates summary statistics (non-parallel)
#    cluster   : Used together with TYPE argument to build a qsub job
#                and distribute across cluster
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

#Master template
MASTER=./src/Y.HPC_scripts/bMaster.sh

#Default target
default: help status

#-------------------------------------
#NMME
NMME NMME_Ensmean SumStats Decadal Decadal_Ensmean Observations:
	make cluster TYPE=$@

cluster:  todo $(OKs)
	@TASK_IDS=`paste -s -d "," $(TODO)`; bsub -J PE_$(TYPE)[$$TASK_IDS] -o PE_$(TYPE).%J.%I.out -e PE_$(TYPE).%J.%I.err -n 1 -R "rusage[mem=8GB]" -W 72:00  -N $(MASTER) $(TYPE) 
	
$(OUTDIR)/%.ok: 
	@echo $* >> $(TODO)

collate persistence:
	qsub -N PE_$@ -v NAME=$@ $(MASTER)
	
#-------------------------------------
#Remove any existing To do files
todo: $(TODOs)

$(TODO_DIR)/%.todo: FORCE 
	@cat /dev/null  > $@ 

status: $(addsuffix .status,$(TYPES))

%.status: $(CFG_DIR)/%.cfg
	@echo `printf "%-50s" "$*"`  : `ls $(CFG_DIR)/$*/ | wc -l` out of `grep -c "^[0-9]\+," $<` jobs

setup: FORCE
	-mkdir $(TYPE_DIRS)
	-mkdir $(TODO_DIR)

install: FORCE
	Rscript src/A1.Setup_system.r

NMME_sync:
	bsub  < src/Y.HPC_scripts/bNMME_sync.sh

#-------------------------------------
clean:
	-@rm PE_* -f

purge:
	-@rm $(addsuffix /*,$(TYPE_DIRS))
	-@rm $(TODO_DIR)/*

FORCE:

vars:
	echo $(TODO)

#Targets
help:
	@#Display help up to first empty line
	@grep "^ *$$" Makefile -n -m1 |   sed "s/\(^.*\):.*/\1/" | xargs -I {} head -n{} Makefile


