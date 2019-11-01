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
# Additional targets
#    help      ; Displays Makefile header with list of target
#    reset     : Remove all todo files - specify a subset using TYPE=...
#    clean     : Remove all log files from working directory
#    status    : List of available types and their current status
#    setup     : Creates necessary subdirectories
#    install   : Installs support R packages
#
#    NMME_sync : Downloads NMME data via OpenDAP
#    PPStats   : Collates statistics (non-parallel) and postprocesses them
#
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
JOB_LIST=$(shell grep --no-messages -o "^[0-9]\+" $(CFG_DIR)/$(TYPE).cfg)
OKs=$(addprefix $(OUTDIR)/, $(addsuffix .ok, $(JOB_LIST)))
TODO=$(TODO_DIR)/$(TYPE).todo

ifdef WAIT
WAIT_CMD='-w numdone( $(WAIT),*)'
endif

#Master template
MASTER=./src/Y.HPC_scripts/bMaster.sh

#Default target
default: help status

#-------------------------------------
#Types

$(filter-out Stats PPStats, $(TYPES)):
	make cluster TYPE=$@

cluster:  todo $(OKs)
	@TASK_IDS=`paste -s -d "," $(TODO)`; \
	bsub $(WAIT_CMD) -J PE_$(TYPE)[$$TASK_IDS] -o PE_$(TYPE).%J.%I.out -e PE_$(TYPE).%J.%I.out -n 1 -R "rusage[mem=8GB]" -R "span[hosts=-1]" -W 72:00 $(MASTER) $(TYPE) 
	
$(OUTDIR)/%.ok: 
	@echo $* >> $(TODO)

Stats:	
	@/appl/R/bin/Rscript-3.5.2-sl73 src/H.Statistics/H0.Collate_metadata.r
	make cluster TYPE=$@

PPStats:
	@touch $(CFG_DIR)/$@.cfg
	@mkdir -p $(CFG_DIR)/$@
	@TYPE=$@; bsub $(WAIT_CMD) -J PE_$$TYPE -o PE_$$TYPE.%J.%I.out -e PE_$$TYPE.%J.%I.out -n 1 -R "rusage[mem=32GB]" -W 72:00 $(MASTER) $$TYPE 

#-------------------------------------
#Remove any existing To do files
todo: $(TODOs)

$(TODO_DIR)/%.todo: FORCE 
	@cat /dev/null  > $@ 

status: configuration.name  $(sort $(addsuffix .status,$(TYPES)))

configuration.name:
	@cat objects/configuration.name
	@echo " configuration"

%.status: $(CFG_DIR)/%.cfg
	@echo `printf "%-50s" "$*"`  : `ls $(CFG_DIR)/$*/ | wc -l` out of `grep -c "^[0-9]\+," $<` jobs

setup: FORCE
	-mkdir $(TYPE_DIRS)
	-mkdir $(TODO_DIR)

install: FORCE
	/appl/R/bin/Rscript-3.5.2-sl73 src/A1.Setup_system.r

NMME_sync:
	bsub  < src/Y.HPC_scripts/bNMME_sync.sh

NMME:
	JOBID=$$(make NMME_by_sources | awk '/is submitted/{print substr($$2, 2, length($$2)-2);}') ;\
	echo $$JOBID ; \
	make NMME_ensmean WAIT=$$JOBID

Decadal:
	JOBID=$$(make Decadal_by_chunks | awk '/is submitted/{print substr($$2, 2, length($$2)-2);}') ;\
	JOBID2=$$(make Decadal_by_sources WAIT=$$JOBID | awk '/is submitted/{print substr($$2, 2, length($$2)-2);}') ;\
	make Decadal_ensmean WAIT=$$JOBID2

CMIP:	
	make CMIP5_by_sources

PP:
	JOBID=$$(make Stats | awk '/is submitted/{print substr($$2, 2, length($$2)-2);}') ;\
	make PPStats WAIT=$$JOBID
	

#-------------------------------------
clean:
	-@rm PE_* -f
	-@rm NMME_* -f

reset:
	-@find $(CFG_DIR)/$(TYPE) -name "*.ok" -print -delete 

FORCE:

vars:
	@echo $(CFGS)
	@echo $(TYPES)
	@echo $(addsuffix .status,$(TYPES))
	@echo WAIT: $(WAIT_CMD)

#Targets
help:
	@#Display help up to first empty line
	@grep "^ *$$" Makefile -n -m1 |   sed "s/\(^.*\):.*/\1/" | xargs -I {} head -n{} Makefile


