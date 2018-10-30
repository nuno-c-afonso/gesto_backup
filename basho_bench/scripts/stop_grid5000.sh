#!/bin/bash

USER="nuafonso"
JOBS_START=2

stop_location () {
    location=$1

    echo "Location: $location"
    counter=1

    ssh -o StrictHostKeyChecking=no $USER@$location 'oarstat -u' | while read line;
    do
        if [ $counter -gt $JOBS_START ]
        then
            res=($(echo $line | grep -Eo "[0-9]*"))
            job_id=${res[0]}
            echo -e "\tStopping job: $job_id"
            ssh $USER@$location "oardel $job_id" >/dev/null &
            sleep 0.2
        fi
        let counter=counter+1
    done
}

# Get locations
LOCATIONS="lille sophia lyon grenoble nancy luxembourg nantes rennes"

# Launch all the necessary shells
for location in $LOCATIONS
do
    stop_location $location &
    # sleep 5
done

# Wait until there are no more jobs running
running=$(jobs -r | wc -l)
while [ $running -ne 0 ]
do
    echo -en "\e[0K\rWaiting for $running locations..."
    running=$(jobs -r | wc -l)
    sleep 0.2
done

echo "Done!"