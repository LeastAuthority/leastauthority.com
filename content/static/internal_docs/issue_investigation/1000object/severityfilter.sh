#! /bin/bash

# Run this script in a directory that contains only flogfiles
# Output a new directory with the time filtered subset

SEVERITY=${1}
OUTDIRECTORY=./severity_filtered_incidents_above_${SEVERITY}
mkdir ${OUTDIRECTORY}

for flogfile in `ls -1 incident-*`
do
    /home/arc/foolscaprepo/bin/flogtool filter --above=${SEVERITY} ${flogfile} ${OUTDIRECTORY}/${flogfile}
done
