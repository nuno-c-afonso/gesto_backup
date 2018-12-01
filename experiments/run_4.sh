#!/bin/bash

# Recover args
NODES=$1
BENCH=$2
MANAGER=$3

# Run-specific variables
BUCKET_FILE="seven_leafs_buckets_dc.txt"
TREE_FILE="seven_gesto.txt"
BENCHMARK_CONFIG_FILE="examples/saturn_benchmarks_rpc.config"
BENCHMARK_REPORT_TAG="report_interval"

# For being able to connect to other machines
KEYNAME=grid5000_internal
add_ssh_key="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"

function run_benchmark {
    driver=$1
    ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE $TREE_FILE $driver 10 70 20 10 0 90"
}

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

# Decrease the report time
cp $BENCHMARK_CONFIG_FILE ${BENCHMARK_CONFIG_FILE}_original
line_to_replace=$(cat $BENCHMARK_CONFIG_FILE | grep $BENCHMARK_REPORT_TAG)
new_line=$(echo "$line_to_replace" | sed "s/^%//g")
sed -i "s|$line_to_replace|$new_line|g" $BENCHMARK_CONFIG_FILE
scripts/update_basho_src.sh basho_bench/examples basho_bench/examples "$BENCH $MANAGER"

# Remove the recovery of all percentiles on migrations (we just need the 95th)
mv src/basho_bench_stats_writer_csv.erl src/basho_bench_stats_writer_csv.erl.OLD
cp ../adapted_deps/basho_bench_stats_writer_csv.erl src
scripts/update_basho_src.sh basho_bench/src basho_bench/src "$BENCH $MANAGER"

# ##############
# ## EVENTUAL ##
# ##############
# scripts/update_leaf_src.sh eventual saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_eventual"

# ##################
# ## GESTO NORMAL ##
# ##################
# scripts/update_leaf_src.sh gesto_partial_concurrent saturn_leaf "$NODES"
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent"

# FIXME: This is just a run for debugging the code!
# TODO: For now, the initial client is still valid. In the future, develop a new one for coping with the directory service.
# gesto_partial_reconfiguration_dir_service
##########################
# GESTO RECONFIGURATION ##
##########################
# This is the special case that needs to sync the events with basho
scripts/update_leaf_src.sh gesto_partial_reconfiguration saturn_leaf "$NODES"
run_benchmark "gesto_benchmarks_reconfig"

# Reset the report time for the other benchmarks
mv ${BENCHMARK_CONFIG_FILE}_original $BENCHMARK_CONFIG_FILE
scripts/update_basho_src.sh basho_bench/examples basho_bench/examples "$BENCH $MANAGER"

# Reset the percentiles calculation in the migrations
mv src/basho_bench_stats_writer_csv.erl.OLD src/basho_bench_stats_writer_csv.erl
scripts/update_basho_src.sh basho_bench/src basho_bench/src "$BENCH $MANAGER"

# Return to the starting point
cd $current_dir
