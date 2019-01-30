#!/bin/sh 

#Som preliminaries for good measure
echo "=========================================================="
echo "Starting on       : $(date)"
echo "Running on node   : $(hostname)"
echo "PBS JOB ID        : $PBS_JOBID"
echo "Working directory : $(pwd)"
echo "=========================================================="

#Setup
module load cdo
module load gcc
module load nco
source ~/.bashrc

#Run R 
set -e 
source ./src/Y.HPC_scripts/${1}.sh

#Finished
echo "=========================================================="
echo "Finished on : $(date)"
echo "Elapsed time  : $SECONDS s"
echo "=========================================================="

