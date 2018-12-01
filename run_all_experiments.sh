#!/bin/bash

# The automatic tree file with a broker per region
SATURN_TREEFILE=grid5000_saturn.txt
CLOSEST_REPLICA=gesto_worst_migration.txt
SATURN_WORST=saturn_worst_migration.txt

# Nodes should be running by the open_grid5000.sh
NODES="172.16.48.11 172.16.48.5 172.16.132.6 172.16.177.7 172.16.72.6 172.16.193.8 172.16.97.7 172.16.37.10"
BENCH="172.16.48.4 172.16.132.42 172.16.177.4 172.16.72.51 172.16.193.7 172.16.97.6 172.16.37.4 172.16.48.3 172.16.132.16 172.16.177.16 172.16.72.50 172.16.193.11 172.16.97.5 172.16.37.3"
MANAGER="172.16.132.38"

# For being able to connect to other machines
KEYNAME=grid5000_internal
add_ssh_key="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"
# eval "$(ssh-agent -s)"; ssh-add ~/.ssh/grid5000_internal

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
ssh $machine "echo '`cat scripts/build_saturn_tree_v2.sh`' > basho_bench/scripts/build_saturn_tree_v2.sh; chmod +x basho_bench/scripts/build_saturn_tree_v2.sh; cd basho_bench; scripts/build_saturn_tree_v2.sh $ping_file $SATURN_TREEFILE"
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

############################################################
## MIGRATION CONFIGURATION FILE (FARTHER SATURN REPLICAS) ##
############################################################

# Get the id of each location
let id_rennes=$(echo "$filtered_leaves" | grep -n rennes | grep -Eo "^[0-9]*")-1
let id_nantes=$(echo "$filtered_leaves" | grep -n nantes | grep -Eo "^[0-9]*")-1
let id_lyon=$(echo "$filtered_leaves" | grep -n lyon | grep -Eo "^[0-9]*")-1
let id_sophia=$(echo "$filtered_leaves" | grep -n sophia | grep -Eo "^[0-9]*")-1
let id_nancy=$(echo "$filtered_leaves" | grep -n nancy | grep -Eo "^[0-9]*")-1
let id_luxembourg=$(echo "$filtered_leaves" | grep -n luxembourg | grep -Eo "^[0-9]*")-1
let id_lille=$(echo "$filtered_leaves" | grep -n lille | grep -Eo "^[0-9]*")-1

# Build the file
echo "$id_rennes,$id_luxembourg" > files/saturn/$SATURN_WORST
echo "$id_nantes,$id_luxembourg" >> files/saturn/$SATURN_WORST
echo "$id_lyon,$id_luxembourg" >> files/saturn/$SATURN_WORST
echo "$id_sophia,$id_luxembourg" >> files/saturn/$SATURN_WORST
echo "$id_nancy,$id_rennes" >> files/saturn/$SATURN_WORST
echo "$id_luxembourg,$id_rennes" >> files/saturn/$SATURN_WORST
echo "$id_lille,$id_rennes" >> files/saturn/$SATURN_WORST

# FIXME: Having this part commented may be an origin for issues!
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
experiments/run_1.2_all_systems.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_1.2.sh "$NODES" "$BENCH" "$MANAGER"

############################################
## 2 - VISIBILITY AND MIGRATION LATENCIES ##
############################################
# Remote updates
# experiments/run_2.1_all_systems.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.1.1.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.1.2.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.1.2-v2.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.1.3.sh "$NODES" "$BENCH" "$MANAGER"

# Migration
# experiments/run_2.2_all_systems.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.2.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.2.2.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_2.2.3.sh "$NODES" "$BENCH" "$MANAGER"

#################
## 3 - SCALING ##
#################
# experiments/run_3_all_systems.sh "$NODES" "$BENCH" "$MANAGER"
# experiments/run_3.sh "$NODES" "$BENCH" "$MANAGER"

#########################
## 4 - RECONFIGURATION ##
#########################
# experiments/run_4.sh "$NODES" "$BENCH" "$MANAGER"

##########################
## 5 - IMPACT OF DELAYS ##
##########################

############################
## ADDITIONAL EXPERIMENTS ##
############################
# For knowing the maximum amount of clients supported by each solution
# experiments/run_scalability_clients.sh "$NODES" "$BENCH" "$MANAGER"
