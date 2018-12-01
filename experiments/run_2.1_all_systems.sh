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

# Amount of runs with increasing dependencies
STEP=1
# N_RUNS=7 # Step that completely overloads COPS
N_RUNS=2 # For repeating specific values

function run_benchmark {
    driver=$1
    tree_file=$2
    # n_clients=$3
    n_clients=2

    # Change the amount of clients for each system
    ssh -t $machine "cd basho_bench; sed -i \"s/N\_CLIENTS=[0-9]*/N\_CLIENTS=${n_clients}/g\" scripts/benchmark_manager.sh"

    # Save the original version of the driver
    cp src/${driver}.erl src/${driver}.erl.OLD

    # i=0
    i=1
    while [ $i -lt $N_RUNS ]
    do
        # Change the frequency of compressing updates
        sed -i "s/RESET\_DEPS\_UPDATE\_FREQUENCY, [0-9]*/RESET\_DEPS\_UPDATE\_FREQUENCY, $i/g" src/${driver}.erl
        # Update the bench nodes source
        scripts/update_basho_src.sh basho_bench/src basho_bench/src "$BENCH $MANAGER"
        # Run the new setting
        ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 1 0 0 1 0 0"

        let i+=STEP
    done

    # Restore the original driver
    mv src/${driver}.erl.OLD src/${driver}.erl
}

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

# TODO: Before running this system, check if 'internals' is with all brokers!
###########################
## SATURN - COMPLEX TREE ##
###########################
scripts/update_leaf_src.sh saturn_default saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_migration_force_ratio" $SATURN_TREEFILE

# Save the original 'internals' state
ssh $machine "cd basho_bench/scripts; cp $BROKER_FILE $BROKER_FILE_TEMP"
# Leave just the first internal (located in Lyon)
ssh $machine "cd basho_bench/scripts; cat $BROKER_FILE_TEMP | head -n 1 > $BROKER_FILE"

# #########################
# ## COMPLETELY EVENTUAL ##
# #########################
# scripts/update_leaf_src.sh eventual saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_eventual_force_ratio" $TREE_FILE 20

# ###################
# ## GESTO_PARTIAL ##
# ###################
# scripts/update_leaf_src.sh gesto_partial_concurrent saturn_leaf "$NODES"
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent_force_ratio" $TREE_FILE 20

# ###################################
# ## OCCULT - TEMPORAL COMPRESSION ##
# ###################################
# # Starts counting from the client update
# scripts/update_leaf_src.sh occult_temporal_remote_update saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_occult_temporal_remote_update_force_ratio" $TREE_FILE

# ###################################
# ## COPS WITH PARTIAL REPLICATION ##
# ###################################
# scripts/update_leaf_src.sh cops_vanilla_partial saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE 20

# ############
# ## OCCULT ##
# ############
# # Starts counting from the client update
# scripts/update_leaf_src.sh occult_remote_update saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_occult_remote_update_force_ratio" $TREE_FILE

# Restore the original manager state
ssh $machine "cd basho_bench/scripts; mv $BROKER_FILE_TEMP $BROKER_FILE"

# Return to the starting point
cd $current_dir
