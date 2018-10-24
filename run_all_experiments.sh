#!/bin/bash

# The automatic tree file with a broker per region
SATURN_TREEFILE=grid5000_saturn.txt
CLOSEST_REPLICA=gesto_worst_migration.txt

# Nodes should be running by the open_grid5000.sh
NODES="172.16.48.12 172.16.48.14 172.16.130.33 172.16.176.4 172.16.64.68 172.16.193.19 172.16.99.11 172.16.37.8"
BENCH="172.16.48.11 172.16.130.28 172.16.176.15 172.16.64.5 172.16.193.18 172.16.99.1 172.16.37.7"
MANAGER="172.16.130.25"

# For being able to connect to other machines
KEYNAME=grid5000_internal
add_ssh_key="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"

cd basho_bench

# Setup the manager machine
machine=root@$MANAGER.g5k

####################################
## SATURN TREE CONFIGURATION FILE ##
####################################

# Create the tree file for the execution of Saturn and add it to the local copies of Basho and Saturn
ssh $machine "rm basho_bench/results/*.ping"
ssh $machine "$add_ssh_key; cd basho_bench; scripts/ping_grid5000.sh"
ping_file=$(ssh $machine "cd basho_bench; ls results/*.ping")
ssh $machine "echo '`cat scripts/process_pings.sh`' > basho_bench/scripts/process_pings.sh; chmod +x basho_bench/scripts/process_pings.sh; cd basho_bench; scripts/process_pings.sh $ping_file"
ssh $machine "echo '`cat scripts/build_saturn_tree.sh`' > basho_bench/scripts/build_saturn_tree.sh; chmod +x basho_bench/scripts/build_saturn_tree.sh; cd basho_bench; scripts/build_saturn_tree.sh $ping_file"
scp $machine:basho_bench/$SATURN_TREEFILE .
cp $SATURN_TREEFILE ../saturn_default/rel/files/manager/trees
echo "$(tail -n +2 $SATURN_TREEFILE)" > $SATURN_TREEFILE
mv $SATURN_TREEFILE files/saturn

#####################################################
## MIGRATION CONFIGURATION FILE (CLOSEST REPLICAS) ##
#####################################################

# Create the file for the worst case scenario for Gesto migrations
hostname_file=$(ssh $machine "cd basho_bench; cat ${ping_file}_avg")

# Deconstruct the file to only the internal nodes
filtered_leaves=$(echo "$hostname_file" | grep -Eo "^LEAF-[a-z]*-[0-9]*\.[a-z]*" | uniq)
connections_leaves=$(echo "$hostname_file" | grep -E "LEAF-[a-z]*-[0-9]*\.[a-z]*->LEAF")
connections_sorted=$(echo "$connections_leaves" | sort -gs -k2)
nodes_chosen=""
connections_chosen=""

# Choose the connections with the lowest latency
while read line
do
    sender=$(echo "$line" | grep -Eo "^LEAF-[a-z]*-[0-9]*\.[a-z]*")
    if [[ "$nodes_chosen" != *"$sender"* ]]
    then
        nodes_chosen="$nodes_chosen $sender"
        connections_chosen=$(printf "$connections_chosen\n$line")
    fi
done <<< "$connections_sorted"

# Convert to the text file format
out_file=""
while read line
do
    if [ ! -z "$line" ]
    then
        nodes=$(echo "$line" | grep -Eo "LEAF-[a-z]*-[0-9]*\.[a-z]*")
        ids=( )
        for node in $nodes
        do
            node_id=$(echo "$filtered_leaves" | grep -n "$node" | grep -Eo "^[0-9]*")
            let node_id--
            ids+=( $node_id )
        done
        printf -v out_file "$out_file${ids[0]},${ids[1]}\n"
    fi
done <<< "$connections_chosen"
echo "$out_file" | head -n -1 | sort -g > files/saturn/$CLOSEST_REPLICA

# Update basho_bench to the latest version
scripts/update_basho_src.sh basho_bench basho_bench "$BENCH $MANAGER"

#########################################
## GETTING MORE PERCENTILES FROM BASHO ##
#########################################

# Download the client dependencies
users="local causal eventual"
for node in $BENCH
do
    ssh root@$node.g5k 'cd basho_bench; make deps' &
    for user in $users
    do
        sleep 0.2
        ssh root@$node.g5k "cd /home/$user/basho_bench; make deps" &    
    done
done

# Wait until all clients have their dependencies
running=$(jobs -r | wc -l)
while [ $running -ne 0 ]
do
    running=$(jobs -r | wc -l)
done

# Allow the 90th percentile
for node in $BENCH
do
    scp ../adapted_deps/bear.erl root@$node.g5k:basho_bench/deps/bear/src &
    for user in $users
    do
        sleep 0.2
        scp ../adapted_deps/bear.erl root@$node.g5k:/home/$user/basho_bench/deps/bear/src &
    done
done

# Wait until all clients have received the update
running=$(jobs -r | wc -l)
while [ $running -ne 0 ]
do
    running=$(jobs -r | wc -l)
done

cd ..

################################
## 1 - LATENCY AND THROUGHPUT ##
################################
# experiments/run_1.2.sh "$NODES" "$BENCH" "$MANAGER"

############################################
## 2 - VISIBILITY AND MIGRATION LATENCIES ##
############################################
# Remote updates
# experiments/run_2.1.1.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.1.2.sh "$NODES" "$BENCH" "$MANAGER"
experiments/run_2.1.2-v2.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.1.3.sh "$NODES" "$BENCH" "$MANAGER"

# Migration
# experiments/run_2.2.2.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.2.3.sh "$NODES" "$BENCH" "$MANAGER"

#################
## 3 - SCALING ##
#################
# experiments/run_3.sh "$NODES" "$BENCH" "$MANAGER"

#########################
## 4 - RECONFIGURATION ##
#########################
# experiments/run_4.sh "$NODES" "$BENCH" "$MANAGER"

##########################
## 5 - IMPACT OF DELAYS ##
##########################

