#!/bin/bash

#Copy manuscript files from server

#Configuration
SRV=transfer:/zhome/39/c/38865/Predictability_engine/scratch/
CFGS=("Bluefin" "Blue_whiting_decadal" "Mackerel_summer")

for CFG in "${CFGS[@]}"
do
  rsync -av $SRV/$CFG/configuration.rds ${CFG}_configuration.rds
  rsync -av $SRV/$CFG/${CFG}_results.sqlite . --progress
done

