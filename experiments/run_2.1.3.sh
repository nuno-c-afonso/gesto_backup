#!/bin/bash

# Recover args
NODES=$1
BENCH=$2
MANAGER=$3

# Bucket files
BUCKET_FILE_DC=seven_leafs_buckets_dc.txt

# Tree files
TREE_FILE=seven_gesto.txt
SATURN_TREEFILE=grid5000_saturn.txt

# Basho configuration files
BROKER_FILE="internals"
BROKER_FILE_TEMP="internals_original"

# For being able to connect to other machines
KEYNAME=grid5000_internal
add_ssh_key="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"

function run_benchmark {
    driver=$1
    tree_file=$2
    # ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 10 0 90"
    ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 90 0 10 90 0"
}

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

###########################
## SATURN - COMPLEX TREE ##
###########################
scripts/update_leaf_src.sh saturn_default saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_migration" $SATURN_TREEFILE

# Save the original 'internals' state
ssh $machine "cd basho_bench/scripts; cp $BROKER_FILE $BROKER_FILE_TEMP"
# Leave just the first internal (located in Lyon)
ssh $machine "cd basho_bench/scripts; cat $BROKER_FILE_TEMP | head -n 1 > $BROKER_FILE"

# ###########
# ## GESTO ##
# ###########
# scripts/update_leaf_src.sh gesto_partial_concurrent saturn_leaf "$NODES"
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE

# ##############
# ## EVENTUAL ##
# ##############
# scripts/update_leaf_src.sh eventual saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_eventual" $TREE_FILE

# Restore the original manager state
ssh $machine "cd basho_bench/scripts; mv $BROKER_FILE_TEMP $BROKER_FILE"

# Return to the starting point
cd $current_dir
