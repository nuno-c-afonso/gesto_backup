#!/bin/bash

CHANGE_BENCHMARK=2
CHANGE_PERCENTAGES=3
N_CLIENTS=2
# N_CLIENTS=1

bucket=$1
tree=$2

if [ $# -gt $CHANGE_BENCHMARK ]
then
    new_driver=$3

    # Get the remaining args
    if [ $# -gt $CHANGE_PERCENTAGES ]
    then
        root_update=$4
        root_read=$5
        root_migrate=$6
        foreign_update=$7
        foreign_read=$8
        foreign_migration=$9
    fi

    # Modify the original file
    to_modify=$(grep "scripts/conf_bench.sh" scripts/run_bench_grid5000.sh)
    while read line
    do
        driver=$(echo $line | grep -Eo "[a-zA-Z_0-9]*benchmarks[a-zA-Z_0-9]*")
        user_line=$(echo "$line" | grep -Eo "'?[a-z]*'?\s*$" | sed "s|'||g")
        temp_line=$line
        if [ $# -gt $CHANGE_PERCENTAGES ]
            then
            # Change the operation percentages
            if [[ $user_line = *"root"* ]]
            then
                line_operations=$(echo $line | grep -Eo "uniform[ 0-9]*$driver")
                new_operations="uniform 10000 $root_update 0 $root_read $root_migrate 0 0 $N_CLIENTS $driver"
                temp_line=$(echo $line | sed "s|$line_operations|$new_operations|g")
            elif [[ $user_line = *"eventual"* ]] || [[ $user_line = *"causal"* ]]
            then
                line_operations=$(echo $line | grep -Eo "uniform[ 0-9]*$driver")
                new_operations="uniform 10000 $foreign_update 0 $foreign_read $foreign_migration 0 0 $N_CLIENTS $driver"
                temp_line=$(echo $line | sed "s|$line_operations|$new_operations|g")
            else
                line_operations=$(echo $line | grep -Eo "uniform[ 0-9]*$driver")
                # new_operations="uniform 10000 10 0 90 0 0 0 $N_CLIENTS $driver" # For the migrations
                new_operations="uniform 10000 1 0 9 0 0 0 $N_CLIENTS $driver" # For the migrations
                # new_operations="uniform 10000 0 0 100 0 0 0 $N_CLIENTS $driver"
                # new_operations="uniform 10000 1 0 0 0 0 0 $N_CLIENTS $driver" # For the forced ratio
                temp_line=$(echo $line | sed "s|$line_operations|$new_operations|g")
            fi
        fi
        # Change the driver
        new_line=$(echo $temp_line | sed "s/$driver/$new_driver/g")
        sed -i "s|$line|$new_line|g" scripts/run_bench_grid5000.sh
    done <<< "$to_modify"
fi

# Leaves + internal
scripts/start_leafs.sh;
scripts/change_tree.sh data/manager/trees/$tree;
scripts/change_groups.sh data/manager/buckets/$bucket;
scripts/rel_leafs.sh;
sleep 5; # To give time for the nodes to start
cd scripts/erlang/; ./init_saturntx.sh; cd ../..;

# Bench
scripts/init_bench_grid5000.sh;
scripts/update_groups_bench_grid5000.sh ../../files/$bucket;
scripts/update_groups_reconfiguration.sh ../../files/$bucket;
scripts/update_bucketfull_grid5000.sh ../../files/$bucket ../../files/saturn/$tree;
scripts/run_bench_grid5000.sh;
scripts/summary_grid5000.sh;
