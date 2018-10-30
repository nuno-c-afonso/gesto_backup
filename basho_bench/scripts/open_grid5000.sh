#!/bin/bash

# Get locations
LOCATIONS=($(echo "lyon lille luxembourg nancy nantes rennes sophia sophia"))
NR_LOCATIONS=${#LOCATIONS[@]}
# Sequence of clusters for the different experiments
MACHINES=($(echo "taurus chetemi petitprince grisou ecotype parasilo uvb uvb"))
# Get number of nodes
# Sophia will have replica + clients + manager
NODES_REGION=($(echo "4 3 3 3 3 3 3 1"))

# Renting time (in hours)
WALLTIME=1:00

# All the machines that will be involved in the benchmark
let workers=NR_LOCATIONS-1
# let workers=NR_LOCATIONS
i=0
while [ $i -lt $workers ]
do
    j=0
    while [ $j -lt ${NODES_REGION[$i]} ]
    do
        sleep 3
        xterm -T ${LOCATIONS[$i]} -hold -e "scripts/prepare_grid5000.sh ${LOCATIONS[$i]} $WALLTIME ${MACHINES[$i]}" &
        let j=j+1
    done
    let i=i+1
done

# Manager that controls the benchmark parameters
gnome-terminal --title="MANAGER: ${LOCATIONS[$workers]}" -e "bash -c \"scripts/manager_grid5000.sh ${LOCATIONS[$workers]} $WALLTIME ${MACHINES[$workers]}; bash\"" &
