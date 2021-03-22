#!/bin/sh 
### General options 
### -- specify queue -- 
#BSUB -q hpc
### -- set the job Name and array-- 
#BSUB -J PE_make
### -- ask for number of cores (default: 1) -- 
#BSUB -n 8
### -- specify that the cores must be on the same host -- 
#BSUB -R "span[hosts=1]"
### -- specify that we need 2GB of memory per core/slot -- 
#BSUB -R "rusage[mem=8GB]"
### -- specify that we want the job to get killed if it exceeds 3 GB per core/slot -- 
##BSUB -M 16GB
### -- set walltime limit: hh:mm -- 
#BSUB -W 72:00 
### -- set the email address -- 
# please uncomment the following line and put in your e-mail address,
# if you want to receive e-mail notifications on a non-default address
#BSUB -u mpay@aqua.dtu.dk
### -- send notification at start -- 
##BSUB -B 
### -- send notification at completion -- 
#BSUB -N 
### -- Specify the output and error file. %J is the job-id -- 
### -- -o and -e mean append, -oo and -eo mean overwrite -- 
#BSUB -oo _targets/PE_make.%J.%I.out 
#BSUB -eo _targets/PE_make.%J.%I.out 

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
module load gcc
module load nco
module load gdal/2.2.3

./Rscript ./src/ZZ.Helpers/targets.r local.data

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

