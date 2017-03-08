#! /bin/bash

# Run this script in a directory that contains only flogfiles
# Output a new directory with the time filtered subset

START=${1}
STOP=${2}
OUTDIRECTORY=../incidents_time_filtered_between_${START}_${STOP}
mkdir ${OUTDIRECTORY}

for flogfile in `ls -1`
do
    /home/arc/foolscaprepo/bin/flogtool filter --after=${START} --before=${STOP} ${flogfile} ${OUTDIRECTORY}/${flogfile}
done
