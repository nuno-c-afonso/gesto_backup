#!/bin/bash

# Recover args
NODES=$1
BENCH=$2
MANAGER=$3

# Run-specific variables
MIN_REPLICAS=4
# MIN_REPLICAS=6
# MIN_REPLICAS=14
# MIN_REPLICAS=21 # Forces all machines to download the dependencies
# MIN_REPLICAS=16 # Forces all machines to download the dependencies

# MAX_REPLICAS=21
MAX_REPLICAS=16
BUCKET_FILE_SUFFIX="_leafs_buckets_dc.txt"
TREE_FILE_SUFFIX="_gesto.txt"
SATURN_TREE_FILE_SUFFIX="_saturn.txt"

# Basho configuration files
BROKER_FILE="internals"
BROKER_FILE_TEMP="internals_original"

# For being able to connect to other machines
KEYNAME=grid5000_internal
add_ssh_key="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"

function choose_n_replicas {
    file=$1
    n_machines=$2

    # Get the different cluster IP addresses
    sites_ip=$(echo "$file" | grep -Eo "[0-9]*\.[0-9]*\.[0-9]*" | uniq)
    n_sites=$(echo "$sites_ip" | wc -w)
    # n_machines=$(cat $file | wc -w)

    # Fill the array with the amount of replicas per cluster
    n_replicas=( )
    i=0
    while [ $i -lt $n_sites ]
    do
        if [ $i -lt $n_machines ]
        then
            n_replicas+=( 1 )
        else
            n_replicas+=( 0 )
        fi

        let i++
    done

    # Add multiple machines in each site
    if [ $n_machines -gt $n_sites ]
    then
        i=$n_sites

        while [ $i -lt $n_machines ]
        do
            # Increment the amount of machines for the following run
            inc_index=$(( i % n_sites  ))
            n_replicas[$inc_index]=$(( n_replicas[inc_index] + 1 ))
            let i++
        done
    fi

    # Pick the replicas for the current run
    chosen_machines=""
    j=0
    for ip in $sites_ip
    do
        n_replicas_site=${n_replicas[$j]}
        if [ $n_replicas_site -gt 0 ]
        then
            printf -v chosen_machines "$chosen_machines$(echo "$file" | grep -m$n_replicas_site $ip)\n"
        fi

        let j++
    done

    # Output the new file
    echo "$chosen_machines"
}

function run_benchmark {
    driver=$1
    tree_file_suffix=$2
    all_leaves=$(ssh $machine 'cat basho_bench/scripts/leafs_backup')
    all_clients=$(ssh $machine 'cat basho_bench/scripts/bench_backup')
    all_internals=$(ssh $machine 'cat basho_bench/scripts/internals_backup')

    # Iterate until there is one replica per location
    counter=$MIN_REPLICAS
    while [ $counter -le $MAX_REPLICAS ]
    do
        # Recover the configuration files
        bucket_file="${counter}${BUCKET_FILE_SUFFIX}"
        tree_file="${counter}${tree_file_suffix}"
        
        # Change leaves and bench
        test_leaves=$(choose_n_replicas "$all_leaves" $counter)
        test_clients=$(choose_n_replicas "$all_clients" $counter)

        # Change the available leaves and clients at the manager
        ssh $machine "echo '$test_leaves' > basho_bench/scripts/leafs"
        ssh $machine "echo '$test_clients' > basho_bench/scripts/bench"

        # # FIXME: This is just run for Saturn!
        # # Create the tree file for the execution of Saturn and add it to the local copies of Basho and Saturn
        # test_internals=$(echo "$all_internals" | head -$counter)
        # ssh $machine "echo '$test_internals' > basho_bench/scripts/internals"
        # ssh $machine "rm basho_bench/results/*.ping"
        # ssh $machine "$add_ssh_key; cd basho_bench; scripts/ping_grid5000.sh"
        # ping_file=$(ssh $machine "cd basho_bench; ls results/*.ping")
        # ssh $machine "cd basho_bench; scripts/process_pings.sh $ping_file"
        # ssh $machine "cd basho_bench; scripts/build_saturn_tree_v2.sh $ping_file $tree_file"
        # scp $machine:basho_bench/$tree_file .
        # cp $tree_file ../saturn_default/rel/files/manager/trees
        # echo "$(tail -n +2 $tree_file)" > $tree_file
        # mv $tree_file files/saturn
        # # Update the clients and the nodes
        # scripts/update_basho_src.sh basho_bench/files basho_bench/files "$BENCH $MANAGER"
        # scripts/update_leaf_src.sh saturn_default/rel saturn_leaf/rel "$NODES"

        # Run the test
        # ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $bucket_file $tree_file $driver 1 0 0 1 0 0"
        ssh -t $machine "$add_ssh_key; cd basho_bench; scripts/benchmark_manager.sh $bucket_file $tree_file $driver 1 9 0 1 9 0"

        let counter++
    done
}

# Store the starting point
current_dir=$(pwd)
cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

# Save the original 'internals' state
ssh $machine "cd basho_bench/scripts; cp $BROKER_FILE $BROKER_FILE_TEMP"
# Leave just the first internal (located in Lyon)
ssh $machine "cd basho_bench/scripts; cat $BROKER_FILE_TEMP | head -n 1 > $BROKER_FILE"

# ###################################
# ## COPS WITH PARTIAL REPLICATION ##
# ###################################
# # It is the first, because it bottlenecks more easily
# scripts/update_leaf_src.sh cops_vanilla_partial saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_vanilla_cops_partial_force_ratio_v2" $TREE_FILE_SUFFIX
# # run_benchmark "saturn_benchmarks_da_vanilla_cops_partial" $TREE_FILE_SUFFIX

###################
## GESTO_PARTIAL ##
###################
scripts/update_leaf_src.sh gesto_partial_concurrent saturn_leaf "$NODES"
# run_benchmark "gesto_benchmarks_migration_partial_free_concurrent_force_ratio" $TREE_FILE_SUFFIX
run_benchmark "gesto_benchmarks_migration_partial_free_concurrent" $TREE_FILE_SUFFIX

#########################
## COMPLETELY EVENTUAL ##
#########################
scripts/update_leaf_src.sh eventual saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_eventual_force_ratio" $TREE_FILE_SUFFIX
run_benchmark "saturn_benchmarks_da_eventual" $TREE_FILE_SUFFIX

############
## OCCULT ##
############
# Starts counting from the client update
scripts/update_leaf_src.sh occult_remote_update saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_occult_remote_update_force_ratio" $TREE_FILE_SUFFIX
run_benchmark "saturn_benchmarks_da_occult" $TREE_FILE_SUFFIX

###################################
## OCCULT - TEMPORAL COMPRESSION ##
###################################
# Starts counting from the client update
scripts/update_leaf_src.sh occult_temporal_remote_update saturn_leaf "$NODES"
# run_benchmark "saturn_benchmarks_da_occult_temporal_remote_update_force_ratio" $TREE_FILE_SUFFIX
run_benchmark "saturn_benchmarks_da_occult_temporal" $TREE_FILE_SUFFIX

# Restore the original manager state
ssh $machine "cd basho_bench/scripts; mv $BROKER_FILE_TEMP $BROKER_FILE"

# TODO: Before running this system, check if 'internals' is with all brokers!
# ###########################
# ## SATURN - COMPLEX TREE ##
# ###########################
# scripts/update_leaf_src.sh saturn_default saturn_leaf "$NODES"
# # run_benchmark "saturn_benchmarks_da_migration_force_ratio" $SATURN_TREE_FILE_SUFFIX
# run_benchmark "saturn_benchmarks_da_migration" $SATURN_TREE_FILE_SUFFIX

# Return to the starting point
cd $current_dir
