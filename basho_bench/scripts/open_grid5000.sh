#!/bin/bash

# Get locations
# Get number of nodes
# Sequence of cluster for the different experiments
# Sophia will have replica + clients + manager

# LOCATIONS=($(echo "lyon lyon lille lille luxembourg luxembourg nancy nancy nantes nantes rennes rennes sophia sophia sophia"))
# MACHINES=($(echo "nova taurus chetemi chifflet petitprince granduc grisou graphene ecotype econome paravance parasilo uvb suno suno"))
# NODES_REGION=($(echo "4 3 4 3 4 3 4 3 4 3 4 3 4 3 1"))

LOCATIONS=($(echo "lyon lille luxembourg nancy nantes rennes sophia sophia"))
# # MACHINES=($(echo "taurus chetemi granduc griffon ecotype paravance suno suno"))
# MACHINES=($(echo "nova chetemi petitprince grisou ecotype paravance suno suno"))

# DO NOT ERASE!!! THESE WERE THE MACHINES FOR THE EVALUATION ON THE THESIS (WITH SCALABILITY)!!!
# MACHINES=($(echo "orion chetemi petitprince graphite ecotype parapluie suno suno"))
# For getting the scalability numbers (also for the thesis)
MACHINES=($(echo "taurus chetemi petitprince grisou ecotype parasilo uvb uvb"))

# # NODES_REGION=($(echo "3 2 2 2 2 2 2 1"))
# NODES_REGION=($(echo "4 3 3 3 3 3 3 1"))
# # NODES_REGION=($(echo "4 3 3 3 3 2 3 1"))
# NODES_REGION=($(echo "3 3 3 3 3 3 3 1"))
# NODES_REGION=($(echo "4 4 4 4 4 4 4 1"))
NODES_REGION=($(echo "4 3 3 3 3 3 3 1"))

# LOCATIONS=($(echo "sophia sophia"))
# MACHINES=($(echo "uvb uvb"))
# NODES_REGION=($(echo "3 1 "))

# LOCATIONS=($(echo "lyon lille luxembourg nancy nantes rennes sophia"))
# MACHINES=($(echo "nova chetemi granduc griffon ecotype paravance suno"))
# NODES_REGION=($(echo "2 2 2 2 2 2 2"))

# LOCATIONS=($(echo "lyon lille luxembourg nancy nantes rennes sophia"))
# MACHINES=($(echo "taurus chetemi granduc griffon econome paranoia suno"))
# NODES_REGION=($(echo "1 1 1 1 1 1 1"))

# LOCATIONS=($(echo "lille luxembourg nancy nantes rennes sophia"))
# MACHINES=($(echo "chetemi granduc griffon ecotype paravance uvb"))
# NODES_REGION=($(echo "1 1 1 1 1 1"))

# LOCATIONS=($(echo "sophia"))
# MACHINES=($(echo "suno"))
# NODES_REGION=($(echo "1"))

NR_LOCATIONS=${#LOCATIONS[@]}

# Older sequence - DO NOT USE!!!
# MACHINES=($(echo "nova chetemi granduc grisou ecotype paravance suno suno"))

# Renting time (in hours)
# WALLTIME=7:00
WALLTIME=1:00
# WALLTIME=63:20
# WALLTIME=10:30
# WALLTIME=15:45

# All the machines that will be involved in the benchmark
let workers=NR_LOCATIONS-1
# let workers=NR_LOCATIONS
i=0
while [ $i -lt $workers ]
do
    j=0
    while [ $j -lt ${NODES_REGION[$i]} ]
    do
        # sleep 0.2
        # sleep 1
        sleep 3
        xterm -T ${LOCATIONS[$i]} -hold -e "scripts/prepare_grid5000.sh ${LOCATIONS[$i]} $WALLTIME ${MACHINES[$i]}" &
        let j=j+1
    done
    let i=i+1
done

# Manager that controls the benchmark parameters
gnome-terminal --title="MANAGER: ${LOCATIONS[$workers]}" -e "bash -c \"scripts/manager_grid5000.sh ${LOCATIONS[$workers]} $WALLTIME ${MACHINES[$workers]}; bash\"" &
