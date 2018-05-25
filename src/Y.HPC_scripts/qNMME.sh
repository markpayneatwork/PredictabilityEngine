#!/bin/bash -x
#Set default job name
#PBS -N PE_NMME 
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

#Run R 
set -e 
Rscript src/E.NMME/E1.Retrieve_NMME_archive_metadata.r
Rscript src/E.NMME/E2.Retrieve_NMME_data.r
Rscript src/E.NMME/E3.Explode_downloaded_data.r
Rscript src/E.NMME/E4.Collect_fragments_metadata.r
Rscript src/E.NMME/E5.Climatologies.r
Rscript src/E.NMME/E6.Calculate_anomalies.r
Rscript src/E.NMME/E7.Calculate_realization_means.r
Rscript src/E.NMME/E8.Calculate_NMME_ensemble_mean.r

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

