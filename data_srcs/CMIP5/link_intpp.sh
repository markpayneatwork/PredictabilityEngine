#/bin/bash
VAR=intpp
find /SCRATCH/mpay/CMIP/$VAR/ -name *.nc  -exec ln -sf {} $VAR/ \;

#Remove links with dates beyond 2100
find $VAR  -name "*_2[1-9]*.nc" -delete

