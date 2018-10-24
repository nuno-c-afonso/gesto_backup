#!/bin/bash

BENCHMARK_HOME=/home/nca/Desktop/gesto-sandbox/saturn_benchmark
SATURN_LEAF_HOME=$BENCHMARK_HOME/saturn_leaf
BASHO_HOME=$BENCHMARK_HOME/basho_bench
USER=nuafonso
HOST=access.grid5000.fr

# Get locations
locations=$(ssh $USER@$HOST "ls")
locations=($locations)

# Avoid copying binary files
sudo rm -rf $SATURN_LEAF_HOME/deps
sudo rm -rf $SATURN_LEAF_HOME/ebin
sudo rm -rf $BASHO_HOME/deps
sudo rm -rf $BASHO_HOME/ebin

# Transfer to one location
first_location=${locations[0]}
scp -rp $SATURN_LEAF_HOME $USER@$HOST:$first_location
scp -rp $BASHO_HOME $USER@$HOST:$first_location

# Spread over all other locations
nr_locations=${#locations[@]}
i=1
while [ $i -lt $nr_locations ]
do
    ssh $USER@$HOST "cp -rf $first_location/saturn_leaf ${locations[$i]}"
    ssh $USER@$HOST "cp -rf $first_location/basho_bench ${locations[$i]}"    
    let i=i+1
done
