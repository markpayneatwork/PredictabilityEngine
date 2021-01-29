#!/bin/bash

#Copy manuscript files from server

#Configuration
SRV=transfer:/zhome/39/c/38865/Predictability_engine/scratch/
CFGS=("Bluefin" "Blue_whiting_decadal" "Mackerel_summer" "NA_Sal" "NA_SST")
FILES_FROM=these.files

rm $FILES_FROM

for CFG in "${CFGS[@]}"
do
  echo "$CFG/configuration.rds" >> $FILES_FROM
  echo "$CFG/${CFG}_results.sqlite" >> $FILES_FROM
done

rsync -av --files-from=$FILES_FROM $SRV . --progress 

