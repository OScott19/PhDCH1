#!/bin/bash

WGET='/usr/oscott/PHDCh1/Data/HYCOM'

YEAR='1993'
MONTH='01'
DAY='01'
StartSeq='0'
EndSeq='2'

NCSS='http://ncss.hycom.org/thredds/ncss'
MODEL='GLBv0.08'
EXPT='expt_53.X/data/1994'

VARS="var=salinity_bottom&var=water_temp_bottom&var=salinity&var=water_temp"
#SPATIAL='spatial=bb'
NORTH='north=-50'
SOUTH='south=-67'
EAST='east=-20'
WEST='west=-70'

for PlusDay in `seq $StartSeq $EndSeq`; do

  MyStartTime=`date -d "$YEAR-$MONTH-$DAY +$PlusDay days" +%Y-%m-%dT%H:%M:%SZ`
  MyEndTime=`date -d "$YEAR-$MONTH-$DAY +$PlusDay days" +%Y-%m-%dT%H:%M:%SZ`
  TimeStart="time_start=$MyStartTime"
  TimeEnd="time_end=$MyEndTime"

  echo $PlusDay
  echo $MyStartTime $MyEndTime
done

  OutFile=$MODEL"_"$EXPT"_`echo $MyTime | cut -d 'T' -f 1`T00Z.nc"

  URL="$NCSS/$MODEL/$EXPT?$VARS&$NORTH&$SOUTH&$EAST&$WEST&disableProjSubset=on&horizStride-1&time_start=1994-01-01T12%3A00%3A00Z&time_end=1994-01-05&T09%3A00%3A00Z$&timeStride=1&vertStride=39&addLatLon=true&accept=netcdf4"
  #URL="$NCSS/$MODEL/$EXPT?$VARS&$SPATIAL&$NORTH&$SOUTH&$EAST&$WEST&$TimeStart&$TimeEnd&timeStride=1&vertCoord=&accept=netcdf4"
#T12%3A00%3A00Z
#T12%3A00%3A00Z
#T09%3A00%3A00Z
#T09%3A00%3A00Z

  if [ -s $OutFile ]; then
      echo "[warning] File $OutFile exists (skipping)"
  else
      wget -O $OutFile "$URL"
  fi
done
