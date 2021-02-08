#!/bin/bash

#Copy manuscript files from server

#Configuration
SRV=transfer:/zhome/39/c/38865/Predictability_engine/scratch/
FILES_FROM=these.files

rm $FILES_FROM


CFGS=("Bluefin" "Blue_whiting_decadal" "Mackerel_summer")
for CFG in "${CFGS[@]}"
do
  echo "$CFG/configuration.rds" >> $FILES_FROM
  echo "$CFG/${CFG}_statistics.sqlite" >> $FILES_FROM
  echo "$CFG/${CFG}_metrics.sqlite" >> $FILES_FROM
done

CFGS=("NA_Sal" "NA_SST")

for CFG in "${CFGS[@]}"
do
  echo "$CFG/configuration.rds" >> $FILES_FROM
  echo "$CFG/${CFG}_metricsField.sqlite" >> $FILES_FROM
done

rsync -av --files-from=$FILES_FROM $SRV . --progress 

