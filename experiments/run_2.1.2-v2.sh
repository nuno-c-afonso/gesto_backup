#!/bin/bash

# TODO: Before running this benchmark, change the Erlang script for recovering
# the remote update latency

# Recover args
NODES=$1
BENCH=$2
MANAGER=$3

# Bucket files
BUCKET_FILE_FULL=seven_leafs_buckets_full.txt

# Tree files
TREE_FILE=seven_gesto.txt

# For being able to connect to other machines
KEYNAME=grid5000_internal
add_ssh_key="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"

# For updating the local client line
COL_UPDATE=6
COL_READ=8

# Number of reads before an update
# STEP=50
# STEP_MAX=150
STEP=1
# STEP_MAX=15
STEP_MAX=57
N_UPDATES=1
# n_reads=( 25 50 100 150 )
n_reads=( )

# Build the array
# i=$STEP
i=57
while [ $i -le $STEP_MAX ]
do
    n_reads+=($i)
    let i+=$STEP
done

function run_benchmark {
    driver=$1

    # Repeat the experiment for the different read percentages
    for r_number in ${n_reads[@]}
    do
        # Build the new line
        old_line=$(ssh $machine 'grep -m1 "scripts/conf_bench.sh" basho_bench/scripts/run_bench_grid5000.sh')
        new_line=""
        col_counter=0
        for col in $old_line
        do
            let col_counter++

            # Fix the amount of updates to 1
            if [ $col_counter -eq $COL_UPDATE ]
            then
                new_line="${new_line}$N_UPDATES "
            elif [ $col_counter -eq $COL_READ ]
            then
                new_line="$new_line$r_number "
            else
                new_line="$new_line$col "
            fi
        done

        # Replace it in the file
        ssh $machine "sed -i \"s|$old_line|$new_line|g\" basho_bench/scripts/run_bench_grid5000.sh"

        # Run the benchmark
        ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_FULL $TREE_FILE $driver 10 70 20 10 0 90"
    done
}

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

# Remove the special percentiles calculations, because we are focused on the remote visibility latency
mv src/basho_bench_stats_writer_csv.erl src/basho_bench_stats_writer_csv.erl.OLD
cp ../adapted_deps/basho_bench_stats_writer_csv.erl src
scripts/update_basho_src.sh basho_bench/src basho_bench/src "$BENCH $MANAGER"

# #########################
# ## COMPLETELY EVENTUAL ##
# #########################
# scripts/update_leaf_src.sh eventual_remote_updates saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_eventual_remote_updates"

##################
## COPS VANILLA ##
##################
scripts/update_leaf_src.sh cops_vanilla_remote_updates saturn_leaf "$NODES"
run_benchmark "saturn_benchmarks_da_vanilla_cops"

# ################
# ## GESTO_FULL ##
# ################
# scripts/update_leaf_src.sh gesto_partial_concurrent_remote_updates saturn_leaf "$NODES"
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent_remote_updates"

# Reset the percentiles calculation in the migrations
mv src/basho_bench_stats_writer_csv.erl.OLD src/basho_bench_stats_writer_csv.erl
scripts/update_basho_src.sh basho_bench/src basho_bench/src "$BENCH $MANAGER"

# Return to the starting point
cd $current_dir
