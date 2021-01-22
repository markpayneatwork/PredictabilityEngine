#!/bin/bash
ln -sf $1
BNAME=`basename $1 .r`
rm _targets -f
ln -sf scratch/$BNAME/_targets
