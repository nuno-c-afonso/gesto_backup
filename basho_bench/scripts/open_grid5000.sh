#!/bin/bash

# Get locations
# Sophia will have replica + clients + manager
LOCATIONS=($(echo "lyon grenoble sophia sophia"))
# LOCATIONS=($(echo "lyon rennes sophia sophia"))
# LOCATIONS=($(echo "rennes grenoble sophia sophia"))
# LOCATIONS=($(echo "nancy lille luxembourg luxembourg"))

# MACHINES=($(echo "taurus taurus taurus edel genepi suno suno suno"))
# MACHINES=($(echo "taurus taurus taurus genepi genepi suno suno suno"))
# MACHINES=($(echo "hercule hercule hercule edel genepi suno suno suno"))
# MACHINES=($(echo "nova nova nova paranoia paranoia suno suno suno"))
# MACHINES=($(echo "orion orion orion edel genepi suno suno suno"))
# MACHINES=($(echo "parapide parapide parapide genepi genepi uvb uvb uvb"))
# MACHINES=($(echo "graphene graphene graphene chifflet chifflet granduc granduc granduc"))
MACHINES=($(echo "taurus taurus taurus genepi genepi uvb uvb uvb"))

NR_LOCATIONS=${#LOCATIONS[@]}

# Get number of nodes
NODES_REGION=($(echo "3 2 2 1"))

# Renting time (in hours)
WALLTIME=4:00

# All the machines that will be involved in the benchmark
let workers=NR_LOCATIONS-1
i=0
k=0
while [ $i -lt $workers ]
do
    j=0
    while [ $j -lt ${NODES_REGION[$i]} ]
    do
        xterm -T ${LOCATIONS[$i]} -hold -e "scripts/prepare_grid5000.sh ${LOCATIONS[$i]} $WALLTIME ${MACHINES[$k]}" &
        let j=j+1
        let k=k+1
    done
    let i=i+1
done

# Manager that controls the benchmark parameters
gnome-terminal --title="MANAGER: ${LOCATIONS[$workers]}" -e "bash -c \"scripts/manager_grid5000.sh ${LOCATIONS[$workers]} $WALLTIME ${MACHINES[$k]}; bash\"" &
