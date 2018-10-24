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

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

###########################
## SATURN - COMPLEX TREE ##
###########################
scripts/update_leaf_src.sh saturn_default saturn_leaf "$NODES"
tree_file=$SATURN_TREEFILE
# Worst case - Gesto + Occult
driver="saturn_benchmarks_worst_gesto"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Worst case - Saturn ----> saturn_worst in the Erlang's configuration file
driver="saturn_benchmarks_da_migration"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Average case
driver="saturn_benchmarks_da_migration"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 5 45 50"

# Save the original 'internals' state
ssh $machine "cd basho_bench/scripts; cp $BROKER_FILE $BROKER_FILE_TEMP"
# Leave just the first internal (located in Lyon)
ssh $machine "cd basho_bench/scripts; cat $BROKER_FILE_TEMP | head -n 1 > $BROKER_FILE"

##########################
## SATURN - SIMPLE STAR ##
##########################
# Same source as in the Saturn with the complex tree
tree_file=$TREE_FILE
# Worst case - Gesto + Occult
driver="saturn_benchmarks_worst_gesto"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Worst case - Saturn
driver="saturn_benchmarks_da_migration"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Average case
driver="saturn_benchmarks_da_migration"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 5 45 50"

############
## OCCULT ##
############
scripts/update_leaf_src.sh occult saturn_leaf "$NODES"
tree_file=$TREE_FILE
# Worst case - Gesto + Occult
driver="saturn_benchmarks_da_occult"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Worst case - Saturn
driver="saturn_benchmarks_da_occult"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Average case
driver="saturn_benchmarks_da_occult"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 5 45 50"

###################################
## OCCULT - TEMPORAL COMPRESSION ##
###################################
scripts/update_leaf_src.sh occult_temporal saturn_leaf "$NODES"
tree_file=$TREE_FILE
# Worst case - Gesto + Occult
driver="saturn_benchmarks_da_occult_temporal_worst_gesto"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Worst case - Saturn
driver="saturn_benchmarks_da_occult_temporal"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Average case
driver="saturn_benchmarks_da_occult_temporal"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 5 45 50"

###########
## GESTO ##
###########
scripts/update_leaf_src.sh gesto_partial_concurrent saturn_leaf "$NODES"
tree_file=$TREE_FILE
# Worst case - Gesto + Occult
driver="gesto_benchmarks_migration_partial_free_concurrent_worst_migration"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Worst case - Saturn
driver="gesto_benchmarks_migration_partial_free_concurrent_worst_migration"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Average case
driver="gesto_benchmarks_migration_partial_free_concurrent"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 5 45 50"

##############
## EVENTUAL ##
##############
scripts/update_leaf_src.sh eventual saturn_leaf "$NODES"
tree_file=$TREE_FILE
# Worst case - Gesto + Occult
driver="saturn_benchmarks_eventual_worst_gesto"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Worst case - Saturn
driver="saturn_benchmarks_da_eventual"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 50 0 50"
# Average case
driver="saturn_benchmarks_da_eventual"
ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $BUCKET_FILE_DC $tree_file $driver 10 70 20 5 45 50"

# Restore the original manager state
ssh $machine "cd basho_bench/scripts; mv $BROKER_FILE_TEMP $BROKER_FILE"

# Return to the starting point
cd $current_dir
