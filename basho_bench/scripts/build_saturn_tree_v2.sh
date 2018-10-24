#!/bin/bash

# For having a proper sorting
export LC_NUMERIC=en_US.utf-8

INTERNALS=scripts/internals
LEAVES=scripts/leafs

function get_node_id () {
    node=$1
    list_nodes=$2

    # Convert the string to integer 
    node_number=$(echo "$list_nodes" | grep -n "$node" | grep -Eo "^[0-9]*" )
    echo "$((node_number - 1))"
}

# Get the ping file as argument
if [ $# -lt 2 ]
then
    echo "The script must receive the names of the ping and tree files as arguments."
    exit
else
    PING_FILE=$1
    NEW_TREEFILE=$2
fi

# Output file name
PING_HOSTNAME=$PING_FILE\_avg

# Deconstruct the file to only the internal nodes
hostname_file=$(cat $PING_HOSTNAME)
filtered_leaves=$(echo "$hostname_file" | grep -Eo "^LEAF-[a-z]*-[0-9]*\.[a-z]*" | uniq)
filtered_internals=$(echo "$hostname_file" | grep -Eo "^INTERNAL-[a-z]*-[0-9]*\.[a-z]*" | uniq)
printf -v filtered_nodes "$filtered_leaves\n$filtered_internals"
connections_internals=$(echo "$hostname_file" | grep -E "INTERNAL-[a-z]*-[0-9]*\.[a-z]*->INTERNAL")
connections_leaves_internals=$(echo "$hostname_file" | grep -E "LEAF-[a-z]*-[0-9]*\.[a-z]*->INTERNAL")
connections_sorted=$(echo "$connections_internals" | sort -gs -k2)
connections_leaves_internals_sorted=$(echo "$connections_leaves_internals" | sort -gs -k2)

# Internal connections
# Choose the connections with the lowest latencies, without causing a loop
included_nodes=()
tree=""
while read connection
do
    # Recover the involved nodes
    connection_nodes=( $(echo "$connection" | grep -Eo INTERNAL-[a-z]*-[0-9]*\.[a-z]*) )
    sender=${connection_nodes[0]}
    recv=${connection_nodes[1]}

    # Check if the nodes belong to any group
    sender_group="-1"
    recv_group="-1"
    group_id=0
    nr_groups=${#included_nodes[@]}
    while [ $group_id -lt $nr_groups ]
    do
        # Search for the sender
        if [[ "${included_nodes[$group_id]}" = *"$sender"* ]]
        then
            sender_group=$group_id
        fi

        # Search for the recv
        if [[ "${included_nodes[$group_id]}" = *"$recv"* ]]
        then
            recv_group=$group_id
        fi

        let group_id=group_id+1
    done

    # Check if the connection can be added to the tree
    if ! { [ $sender_group -eq $recv_group ] && [ $sender_group -gt "-1" ]; };
    then
        # Both nodes belong to no group
        if [ $sender_group -eq $recv_group ]
        then
            included_nodes+=("$sender $recv")
        
        # Both nodes belong to a group
        elif [ $sender_group -gt "-1" ] && [ $recv_group -gt "-1" ]
        then
            included_nodes[$sender_group]="${included_nodes[$sender_group]} ${included_nodes[$recv_group]}"
            included_nodes[$recv_group]=""

        # One of the nodes does not belong to a group
        else
            if [ $sender_group -eq "-1" ]
            then
                included_nodes[$recv_group]="${included_nodes[$recv_group]} $sender"
            else
                included_nodes[$sender_group]="${included_nodes[$sender_group]} $recv"
            fi
        fi

        # Add the connection to the tree
        printf -v tree "$tree$connection\n"
    fi

done <<< "$connections_sorted"

# Internal and leaf nodes
chosen_leaves=""
while read connection
do
    # Recover the involved nodes
    sender=$(echo "$connection" | grep -Eo LEAF-[a-z]*-[0-9]*\.[a-z]*)
    recv=$(echo "$connection" | grep -Eo INTERNAL-[a-z]*-[0-9]*\.[a-z]*)

    # Add connection if node was not yet chosen
    if [[ $chosen_leaves != *"$sender"* ]]
    then
        # Include the node in the chosen leaves string
        chosen_leaves="$chosen_leaves $sender"

        # Add the connection to the tree
        printf -v tree "$tree$connection\n"
    fi
done <<< "$connections_leaves_internals_sorted"

# Create the tree file
ip_tree=""
nr_internals=$(wc -l $INTERNALS | grep -Eo "[0-9]*")
nr_leaves=$(wc -l $LEAVES | grep -Eo "[0-9]*")
let nr_nodes=nr_internals+nr_leaves
tree_file_header="$nr_leaves"

# Fill the body - top part with leafs
tree_file_body=()
i=0

# Create the lines for leaves
for leaf in $filtered_leaves
do
    # Convert the string to id
    connection_to_internal=$(echo "$tree" | grep $leaf)
    leaf_id=$(get_node_id "$leaf" "$filtered_nodes")
    internal=$(echo "$connection_to_internal" | sed "s|$leaf->||g" | grep -Eo "^.*:" | sed "s|:||g")
    internal_id=$(get_node_id "$internal" "$filtered_nodes")

    # Build the line
    i=0
    line=""
    while [ $i -lt $nr_nodes ]
    do
        if [ $i -eq $leaf_id ]
        then
            line="${line}-1,"
        elif [ $i -eq $internal_id ]
        then
            line="${line}0,"
        elif [ $i -lt $nr_leaves ]
        then
            line="${line}0,"
        else
            line="${line}-1,"
        fi

        let i++
    done

    # Remove the last marker and add it to the tree
    line=${line::-1}
    tree_file_body+=( $line )
done

# Create the lines for internals
for internal in $filtered_internals
do
    internal_id=$(get_node_id "$internal" "$filtered_nodes")
    node_connections=$(echo "$tree" | grep $internal)

    # Convert the names to ids
    other_nodes=$(echo "$node_connections" | grep -Eo ".*:" | sed "s|$internal||g" | sed "s/[>:]//g" | sed "s/^\-//g" | sed "s/-$//g")
    ids=" "
    for other in $other_nodes
    do
        ids="$ids$(get_node_id "$other" "$filtered_nodes") "
    done
    
    # Build the line
    i=0
    line=""
    while [ $i -lt $nr_nodes ]
    do
        if [[ $ids = *" $i "* ]]
        then
            line="${line}0,"
        else
            line="${line}-1,"
        fi

        let i++
    done

    # Remove the last marker and add it to the tree
    line=${line::-1}
    tree_file_body+=( $line )
done

# Output the file
echo $nr_leaves > $NEW_TREEFILE
for line in ${tree_file_body[@]}
do
    echo "$line" | sed "s| ||g" >> $NEW_TREEFILE
done
