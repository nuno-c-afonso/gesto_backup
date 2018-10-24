#!/bin/bash

USER="nuafonso"
JOBS_START=2

# Get locations
LOCATIONS="grenoble lille luxembourg lyon nancy nantes rennes sophia"

for location in $LOCATIONS
do
    echo "Location: $location"
    counter=1
    ssh $USER@$location 'oarstat -u' | while read line;
    do
        if [ $counter -gt $JOBS_START ]
        then
            res=($(echo $line | grep -Eo "[0-9]*"))
            job_id=${res[0]}
            echo -e "\tStopping job: $job_id"
            ssh $USER@$location "oardel $job_id" >/dev/null &
        fi
        let counter=counter+1
    done
done
