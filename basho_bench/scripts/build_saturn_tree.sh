#!/bin/bash

INTERNALS=scripts/internals
LEAVES=scripts/leafs
NEW_TREEFILE=grid5000_saturn.txt

# Get the ping file as argument
if [ $# -lt 1 ]
then
    echo "The script must receive the name of the ping file as an argument."
    exit
else
    PING_FILE=$1
fi

# Output file name
PING_HOSTNAME=$PING_FILE\_avg

# Deconstruct the file to only the internal nodes
hostname_file=$(cat $PING_HOSTNAME)
filtered_internals=$(echo "$hostname_file" | grep -Eo "^INTERNAL-[a-z]*-[0-9]*\.[a-z]*" | uniq)
connections_internals=$(echo "$hostname_file" | grep -E "INTERNAL-[a-z]*-[0-9]*\.[a-z]*->INTERNAL")
connections_sorted=$(echo "$connections_internals" | sort -gs -k2)

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

# Create the tree file
ip_tree=""
nr_internals=$(wc -l $INTERNALS | grep -Eo "[0-9]*")
nr_leaves=$(wc -l $LEAVES | grep -Eo "[0-9]*")
let nr_nodes=nr_internals+nr_leaves
tree_file_header="$nr_leaves"

# Fill the body
tree_file_body=()
i=0
while [ $i -lt $nr_nodes ]
do
    let local_broker=i+nr_leaves
    let local_replica=i-nr_leaves
    line=""
    j=0

    # Populate each line
    while [ $j -lt $nr_nodes ]
    do
        if [ $local_broker -eq $j ] || ( [ $i -ge $nr_leaves ] && [ $j -eq $local_replica ] )
        then
            line="${line}0,"
        elif [ $i -eq $j ] || [ $i -ge $nr_leaves ]
        then
            line="${line}-1,"
        elif [ $j -lt $nr_leaves ]
        then
            line="${line}0,"
        else
            line="${line}-1,"
        fi

        let j++
    done

    # Remove the last marker and add it to the tree
    line=${line::-1}
    tree_file_body+=( $line )

    let i++
done

# Fill with the internals
while read -r connection
do
    if [[ ! -z "${connection// }" ]]
    then
        # Recover the involved nodes IP address
        
        # Sender
        connection_nodes=( $(echo "$connection" | grep -Eo INTERNAL-[a-z]*-[0-9]*\.[a-z]*) )
        sender=${connection_nodes[0]}
        sender_line=$(grep -E -m1 -n "^$sender" $PING_HOSTNAME | grep -Eo "^[0-9]*")
        let sender_line++
        sender_ip=$(sed -n "${sender_line}p" $PING_FILE | grep -Eo "^[0-9\.]*")
        sender_relative=$(grep -E -m1 -n "^$sender_ip" $INTERNALS | grep -Eo "^[0-9]*")
        let sender_abs=sender_relative+nr_leaves-1

        # Receiver
        recv=${connection_nodes[1]}
        recv_line=$(grep -E -m1 -n "^$recv" $PING_HOSTNAME | grep -Eo "^[0-9]*")
        let recv_line++
        recv_ip=$(sed -n "${recv_line}p" $PING_FILE | grep -Eo "^[0-9\.]*")
        recv_relative=$(grep -E -m1 -n "^$recv_ip" $INTERNALS | grep -Eo "^[0-9]*")
        let recv_abs=recv_relative+nr_leaves-1

        # Unlock the positions in the line

        # Sender
        line=${tree_file_body[$sender_abs]}
        new_line=""
        counter=0
        for el in `echo "$line" | sed "s| ||g" | sed "s|,| |g"`
        do
            if [ $counter -eq $recv_abs ]
            then
                new_line="${new_line}0,"
            else
                new_line="${new_line}${el},"
            fi

            let counter++
        done
        new_line=${new_line::-1}
        tree_file_body[$sender_abs]="$new_line"

        # Receiver
        line=${tree_file_body[$recv_abs]}
        new_line=""
        counter=0
        for el in `echo "$line" | sed "s| ||g" | sed "s|,| |g"`
        do
            if [ $counter -eq $sender_abs ]
            then
                new_line="${new_line}0,"
            else
                new_line="${new_line}${el},"
            fi

            let counter++
        done
        new_line=${new_line::-1}
        tree_file_body[$recv_abs]="$new_line"
    fi
done <<< "$tree"

# Output the file
echo $nr_leaves > $NEW_TREEFILE
for line in ${tree_file_body[@]}
do
    echo "$line" | sed "s| ||g" >> $NEW_TREEFILE
done
