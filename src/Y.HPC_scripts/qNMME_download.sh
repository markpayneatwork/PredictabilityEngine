#!/bin/bash -x
#Set default job name
#PBS -N PE_Download 
#PBS -t 1-7

#Run job in current working directory
cd $PBS_O_WORKDIR

#Some preliminaries for good measure
echo "=========================================================="
echo "Starting on       : $(date)"
echo "Running on node   : $(hostname)"
echo "PBS JOB ID        : $PBS_JOBID"
echo "Working directory : $(pwd)"
echo "=========================================================="

#Setup
module load cdo
module load gcc/8.1.0
module load nco

#Run R 
set -e 

#Select R version
#export PATH=/appl/R/3.4.1/bin/:$PATH

Rscript ./src/E.NMME/E1.Retrieve_NMME_data.r

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

