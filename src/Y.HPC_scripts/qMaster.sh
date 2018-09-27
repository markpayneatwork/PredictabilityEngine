#!/bin/bash -x
#Set default job name
#PBS -N PE_${NAME} 

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

source ./src/Y.HPC_scripts/${NAME}.sh

#Error check
if [ "$?" -eq 0 ]; 
then
    grep "^$PBS_ARRAYID," ./scratch/Job_configuration/${NAME}.cfg >> ./scratch/Job_configuration/${NAME}/$PBS_ARRAYID.ok 
    echo "Successful completion."
else 
    echo "Failure"
fi

#Finished
echo "=========================================================="
echo "Finished on : $(date)"
echo "Elapsed time  : $SECONDS s"
echo "=========================================================="

