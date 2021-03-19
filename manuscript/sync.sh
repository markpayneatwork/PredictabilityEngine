#!/bin/bash

#Copy manuscript files from server

#Configuration
SRV=transfer:/zhome/39/c/38865/Predictability_engine/scratch/
FILES_FROM=these.files

rm $FILES_FROM


CFGS=("Bluefin" "Blue-whiting-decadal" "Mackerel-summer")
for CFG in "${CFGS[@]}"
do
  echo "$CFG/configuration.rds" >> $FILES_FROM
  echo "$CFG/${CFG}_Statistics.sqlite" >> $FILES_FROM
  echo "$CFG/${CFG}_Metrics.sqlite" >> $FILES_FROM
done

CFGS=("NA-Sal" "NA-SST")

for CFG in "${CFGS[@]}"
do
  echo "$CFG/configuration.rds" >> $FILES_FROM
  echo "$CFG/${CFG}_MetricsField.sqlite" >> $FILES_FROM
done

rsync -av --files-from=$FILES_FROM $SRV . --progress 

