#!/bin/bash

# Recover args
NODES=$1
BENCH=$2
MANAGER=$3

# Bucket files
BUCKET_FILE_DC=seven_leafs_buckets_dc.txt

# Tree files
TREE_FILE=seven_gesto.txt

# For being able to connect to other machines
KEYNAME=grid5000_internal
add_ssh_key="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"

function run_benchmark {
    driver=$1
    ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $TREE_FILE $driver 10 70 20 10 0 90"
}

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

###################
## GESTO_PARTIAL ##
###################
scripts/update_leaf_src.sh gesto_partial_concurrent saturn_leaf "$NODES"
run_benchmark "gesto_benchmarks_migration_partial_free_concurrent"

#########################
## COMPLETELY EVENTUAL ##
#########################
scripts/update_leaf_src.sh eventual saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_eventual"

############
## OCCULT ##
############
scripts/update_leaf_src.sh occult saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_occult"

###################################
## OCCULT - TEMPORAL COMPRESSION ##
###################################
scripts/update_leaf_src.sh occult_temporal saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_occult_temporal"

# Return to the starting point
cd $current_dir
