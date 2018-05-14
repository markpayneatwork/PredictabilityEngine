#!/bin/bash -x
#Set default job name
#PBS -N LME_fails 
#PBS -t 14,16,48,50-61
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

#Run R
module load cdo
Rscript src/D.Decadal_forecast_systems/D99.Extract_data_from_hindcasts_and_process-multiple_configs.r 

#Error check
if [ "$?" -eq 0 ]; 
then
    echo "Successful completion."
else
    echo "Failure."
fi

#Finished
echo "=========================================================="
echo "Finished on : $(date)"
echo "Elapsed time  : $SECONDS s"
echo "=========================================================="

