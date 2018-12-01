#!/bin/bash

# Recover args
NODES=$1
BENCH=$2
MANAGER=$3

# Run-specific variables
MIN_REPLICAS=4
MAX_REPLICAS=7
FILE_PREFIX=( "four" "five" "six" "seven" )
BUCKET_FILE_SUFFIX="_leafs_buckets_dc.txt"
TREE_FILE_SUFFIX="_gesto.txt"

# For being able to connect to other machines
KEYNAME=grid5000_internal
add_ssh_key="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"

function run_benchmark {
    driver=$1
    all_leaves=$(ssh $machine 'cat basho_bench/scripts/leafs')

    # For knowing the saturation point of Occult
    # n_clients=5
    # max_clients=25
    # while [ $n_clients -le $max_clients ]
    # do
    #     ssh -t $machine "sed -i \"s/N_CLIENTS=[0-9]*/N_CLIENTS=$n_clients/g\" basho_bench/scripts/benchmark_manager.sh"

        # Iterate until there is one replica per location
        counter=$MIN_REPLICAS
        while [ $counter -le $MAX_REPLICAS ]
        do
            # Recover iteration variables
            let file_index=counter-MIN_REPLICAS
            bucket_file="${FILE_PREFIX[$file_index]}${BUCKET_FILE_SUFFIX}"
            tree_file="${FILE_PREFIX[$file_index]}${TREE_FILE_SUFFIX}"
            test_leaves=$(echo "$all_leaves" | head -$counter)

            # Change the available leaves at the manager
            ssh $machine "echo '$test_leaves' > basho_bench/scripts/leafs"

            # Run the test
            # ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $bucket_file $tree_file $driver 10 70 20 10 0 90"
            # ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $bucket_file $tree_file $driver 0 80 20 0 0 100"
            ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $bucket_file $tree_file $driver 10 90 0 10 90 0"
            # ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $bucket_file $tree_file $driver 0 100 0 0 100 0"

            let counter++
        done

    #     let n_clients++
    # done
}

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

# Remove the special percentiles calculations, because we are focused on the throughput
mv src/basho_bench_stats_writer_csv.erl src/basho_bench_stats_writer_csv.erl.OLD
cp ../adapted_deps/basho_bench_stats_writer_csv.erl src
scripts/update_basho_src.sh basho_bench/src basho_bench/src "$BENCH $MANAGER"

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

# Reset the percentiles calculation in the migrations
mv src/basho_bench_stats_writer_csv.erl.OLD src/basho_bench_stats_writer_csv.erl
scripts/update_basho_src.sh basho_bench/src basho_bench/src "$BENCH $MANAGER"

# Return to the starting point
cd $current_dir
