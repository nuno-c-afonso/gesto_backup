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

# It is just testing the 'average case' of 2.2.2
function run_benchmark {
    driver_just_two_replicas=$1
    driver_dc_any=$2
    driver_normal=$3
    tree_file=$4

    # Only between original replica and datacenter
    ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver_just_two_replicas 10 70 20 5 45 50"

    # To datacenter and then any cloudlet
    ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver_dc_any 10 70 20 5 45 50"

    # No restrictions (simple remote-read)
    ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver_normal 10 70 20 5 45 50"
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
run_benchmark "saturn_benchmarks_da_migration_two_replicas" "saturn_benchmarks_da_migration_dc_any" "saturn_benchmarks_da_migration" $SATURN_TREEFILE

# Save the original 'internals' state
ssh $machine "cd basho_bench/scripts; cp $BROKER_FILE $BROKER_FILE_TEMP"
# Leave just the first internal (located in Lyon)
ssh $machine "cd basho_bench/scripts; cat $BROKER_FILE_TEMP | head -n 1 > $BROKER_FILE"

##########################
## SATURN - SIMPLE STAR ##
##########################
# Same source as in the Saturn with the complex tree
run_benchmark "saturn_benchmarks_da_migration_two_replicas" "saturn_benchmarks_da_migration_dc_any" "saturn_benchmarks_da_migration" $TREE_FILE

############
## OCCULT ##
############
scripts/update_leaf_src.sh occult saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_occult_two_replicas" "saturn_benchmarks_da_occult_dc_any" "saturn_benchmarks_da_occult" $TREE_FILE

###################################
## OCCULT - TEMPORAL COMPRESSION ##
###################################
scripts/update_leaf_src.sh occult_temporal saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_occult_temporal_two_replicas" "saturn_benchmarks_da_occult_temporal_dc_any" "saturn_benchmarks_da_occult_temporal" $TREE_FILE

###########
## GESTO ##
###########
scripts/update_leaf_src.sh gesto_partial_concurrent saturn_leaf "$NODES"
run_benchmark "gesto_benchmarks_migration_partial_free_concurrent_two_replicas" "gesto_benchmarks_migration_partial_free_concurrent_dc_any" "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE

##############
## EVENTUAL ##
##############
scripts/update_leaf_src.sh eventual saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_eventual_two_replicas" "saturn_benchmarks_da_eventual_dc_any" "saturn_benchmarks_da_eventual" $TREE_FILE

# Restore the original manager state
ssh $machine "cd basho_bench/scripts; mv $BROKER_FILE_TEMP $BROKER_FILE"

# Return to the starting point
cd $current_dir
