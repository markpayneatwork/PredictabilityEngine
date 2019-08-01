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
module load nco
module load gdal
source ~/.bashrc

#Run R 
set -e 
source ./src/Y.HPC_scripts/${1}.sh

#Error check
if [ "$?" -eq 0 ]; 
then
    grep "^$LSB_JOBINDEX," ./scratch/Job_configuration/${1}.cfg >> ./scratch/Job_configuration/${1}/$LSB_JOBINDEX.ok 
    echo "Successful completion."
else 
    echo "Failure"
fi

#Finished
echo "=========================================================="
echo "Finished on : $(date)"
echo "Elapsed time  : $SECONDS s"
echo "=========================================================="

