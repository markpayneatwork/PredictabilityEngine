#!/bin/bash -x
#Set default job name
#PBS -N PE_NMME_Ensmean
#Set number of tasks and their ID
#PBS -t 1-4
#Set mail address
#PBS -M mpay@aqua.dtu.dk

#Run job in current working directory
cd $PBS_O_WORKDIR

#Some preliminaries for good measure
echo "=========================================================="
echo "Starting on       : $(date)"
echo "Running on node   : $(hostname)"
echo "PBS JOB ID        : $PBS_JOBID"
echo "Working directory : $(pwd)"
echo "Target script     : $(readlink runme)"
echo "=========================================================="

#Setup
module load cdo
module load gcc/8.1.0
module load nco

#Run R 
set -e 

Rscript ./src/E.NMME/E5.Calculate_NMME_ensemble_mean.r


#Error check
if [ "$?" -eq 0 ]; 
then
    echo "Successful completion."
else 
    echo "Failure"
fi

#Finished
echo "=========================================================="
echo "Finished on : $(date)"
echo "Elapsed time  : $SECONDS s"
echo "=========================================================="

