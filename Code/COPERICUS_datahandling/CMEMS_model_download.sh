# !/bin/bash
# Author: Oenone Scott os20359@essex.ac.uk
# Script: Download suitable sections of the COPERNICUS model
# Arguments: one: list of date boundaries & names
# Date:     9 Nov 2021
## PSEUDO CODE
## vector of appropriate month start & end dates
#
#
#
#while read line
#do
#  echo "Record is : $line"
#done < ../Data/datesforCMEMS.csv
#wc -l ../Data/datesforCMEMS.csv
for line in `cat ../Data/CallScripts.csv`
do
    echo $line
done
