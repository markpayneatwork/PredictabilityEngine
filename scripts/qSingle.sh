#!/bin/bash -x
#Set default job name
#PBS -N SingleRun
#PBS -t 1
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
#Rscript src/C.Observations/C1.EN4_data_extraction.r
Rscript src/D.Model_systems/D1.Extract_data_from_CDO_compatable_hindcasts.r
Rscript src/D.Model_systems/D4.Calculate_indicators_from_CDO_compatible.r

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

