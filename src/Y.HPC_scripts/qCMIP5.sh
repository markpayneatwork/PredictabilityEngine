#!/bin/bash -x
#Set default job name
#PBS -N PE_CMIP5 
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

#R script
set -e 
#Rscript src/F.CMIP5_projections/F1.Fragment_CMIP5.r 
Rscript src/F.CMIP5_projections/F2.Climatologies_anomalies_realmeans.r

#Error check
if [ "$?" -eq 0 ]; 
then
    echo "Successful completion."
fi

#Finished
echo "=========================================================="
echo "Finished on : $(date)"
echo "Elapsed time  : $SECONDS s"
echo "=========================================================="

