#!/bin/bash

# TODO: Before running this script, check if 'internals' is with all brokers!

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
    # Default values for each experiment
    driver=$1
    tree_file=$2

    # Specific for each system
    i=$3
    step=$4
    max=$5

    while [ $i -le $max ]
    do
        # Change the amount of clients for each run
        ssh -t $machine "cd basho_bench; sed -i \"s/N\_CLIENTS=[0-9]*/N\_CLIENTS=$i/g\" scripts/benchmark_manager.sh"
        # Run the new setting
        # ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 1 0 0 1 0 0"
        ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 1 0 9 1 0 9"

        let i+=step
    done
}

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

# ###########################
# ## SATURN - COMPLEX TREE ##
# ###########################
# scripts/update_leaf_src.sh saturn_default saturn_leaf "$NODES"
# # run_benchmark "saturn_benchmarks_da_migration_force_ratio" $SATURN_TREEFILE
# # run_benchmark "saturn_benchmarks_da_migration" $SATURN_TREEFILE 1 1 6
# # run_benchmark "saturn_benchmarks_da_migration" $SATURN_TREEFILE 5 5 35 # Single client machine
# # run_benchmark "saturn_benchmarks_da_migration" $SATURN_TREEFILE 4 4 20
# run_benchmark "saturn_benchmarks_da_migration" $SATURN_TREEFILE 1 1 3
# run_benchmark "saturn_benchmarks_da_migration" $SATURN_TREEFILE 24 4 32

# Save the original 'internals' state
ssh $machine "cd basho_bench/scripts; cp $BROKER_FILE $BROKER_FILE_TEMP"
# Leave just the first internal (located in Lyon)
ssh $machine "cd basho_bench/scripts; cat $BROKER_FILE_TEMP | head -n 1 > $BROKER_FILE"

###########
## GESTO ##
###########
scripts/update_leaf_src.sh gesto_partial_concurrent saturn_leaf "$NODES"
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent_force_ratio" $TREE_FILE
# scripts/update_leaf_src.sh gesto_partial_concurrent_fast_latency saturn_leaf "$NODES"
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE 4 4 20
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE 1 1 3
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE 24 4 32
run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE 28 4 32
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE 5 5 35 # Single client machine
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE 1 2 7
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE 1 1 6

# #########################
# ## COMPLETELY EVENTUAL ##
# #########################
# scripts/update_leaf_src.sh eventual saturn_leaf "$NODES"
# # run_benchmark "saturn_benchmarks_da_eventual_force_ratio" $TREE_FILE
# # run_benchmark "saturn_benchmarks_da_eventual" $TREE_FILE 4 4 20
# run_benchmark "saturn_benchmarks_da_eventual" $TREE_FILE 1 1 3
# run_benchmark "saturn_benchmarks_da_eventual" $TREE_FILE 24 4 32
# # run_benchmark "saturn_benchmarks_da_eventual" $TREE_FILE 5 5 35 # Single client machine
# # run_benchmark "saturn_benchmarks_da_eventual" $TREE_FILE 1 1 6

# ###################################
# ## OCCULT - TEMPORAL COMPRESSION ##
# ###################################
# # Starts counting from the client update
# # scripts/update_leaf_src.sh occult_temporal_remote_update saturn_leaf "$NODES"
# # run_benchmark "saturn_benchmarks_da_occult_temporal_remote_update_force_ratio" $TREE_FILE
# # run_benchmark "saturn_benchmarks_da_occult_temporal_remote_update" $TREE_FILE 13 13 65
# # run_benchmark "saturn_benchmarks_da_occult_temporal_remote_update" $TREE_FILE 9 9 45
# # TODO: It seems that the client machines are becoming a bottleneck!!!
# # run_benchmark "saturn_benchmarks_da_occult_temporal_remote_update" $TREE_FILE 10 10 50
# # It is the normal version
# scripts/update_leaf_src.sh occult_temporal saturn_leaf "$NODES"
# # run_benchmark "saturn_benchmarks_da_occult_temporal" $TREE_FILE 13 13 65
# run_benchmark "saturn_benchmarks_da_occult_temporal" $TREE_FILE 4 4 12
# run_benchmark "saturn_benchmarks_da_occult_temporal" $TREE_FILE 78 13 104
# # run_benchmark "saturn_benchmarks_da_occult_temporal" $TREE_FILE 10 10 60 # Single client machine

# ###################################
# ## COPS WITH PARTIAL REPLICATION ##
# ###################################
# scripts/update_leaf_src.sh cops_vanilla_partial saturn_leaf "$NODES"
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE 6 3 18
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE 1 2 5
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE 21 3 27
# run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE 21 6 27
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE 5 5 35 # Single client machine
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE 20 5 35 # Single client machine
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE 1 1 5
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_relaxed_ratio_v2" $TREE_FILE 6 3 18
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_relaxed_ratio_v2" $TREE_FILE 3 2 12
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_relaxed_ratio_v2" $TREE_FILE 9 6 18
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial" $TREE_FILE 6 3 18

# TODO: It is not necessary to use this for the tests!!! Use just the compressed version!!!
# ############
# ## OCCULT ##
# ############
# # Starts counting from the client update
# # scripts/update_leaf_src.sh occult_remote_update saturn_leaf "$NODES"
# # run_benchmark "saturn_benchmarks_da_occult_remote_update_force_ratio" $TREE_FILE
# # It is the normal version
# scripts/update_leaf_src.sh occult saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_occult" $TREE_FILE

# Restore the original manager state
ssh $machine "cd basho_bench/scripts; mv $BROKER_FILE_TEMP $BROKER_FILE"

# Return to the starting point
cd $current_dir
