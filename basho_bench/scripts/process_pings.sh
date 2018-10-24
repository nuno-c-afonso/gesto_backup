#!/bin/bash

START_IP="172.16."

# Install the necessary packages
sudo apt-get install dnsutils bc

# Get the ping file as argument
if [ $# -lt 1 ]
then
    echo "The script must receive the name of the ping file as an argument."
    exit
else
    PING_FILE=$1
fi

# Output file name
PING_OUT=$PING_FILE\_avg
out_file=$(cat $PING_FILE)

# Get the IPs
ip_serializer=$(grep -m1 $START_IP $PING_FILE | grep -Eo "^[0-9.]*")
ip_others=( $(grep "$ip_serializer->" $PING_FILE | grep -Eo "\->[0-9.]*:" | grep -Eo "[0-9.]*") )
ip_all=$(echo "$ip_serializer ${ip_others[@]}")

# Get number of serializers and leaves
nr_serializers=$(cat scripts/internals | wc -l)
nr_leaves=$(cat scripts/leafs | wc -l)
let nr_nodes=nr_serializers+nr_leaves

# Get the hostname associated with the IP addresses and switch them
counter=0
for ip in $ip_all
do
    # Recover the hostname
    host=$(nslookup $ip | grep -Eo "=.*" | grep -Eo "[a-z].*")
    host=${host::-1}
    host=$(echo $host | grep -Eo "[a-z]*-[0-9]*.[a-z]*")

    # Add the machine function
    if [ $counter -lt $nr_serializers ]
    then
        host="INTERNAL-$host"
    elif [ $counter -lt $nr_nodes ]
    then
        host="LEAF-$host"
    else
        host="CLIENT-$host"
    fi

    # Change the original file
    out_file=$(echo "$out_file" | sed "s|$ip:|$host:|g")
    out_file=$(echo "$out_file" | sed "s|$ip-|$host-|g")
    
    let counter=counter+1
done

# Keep only half the average ping (it is taken as RTT)
rm $PING_OUT
echo "$out_file" | while read line
do
    # Do not print empty lines
    if [[ ! -z "${line// }" ]]
    then
        direction=$(echo $line | grep -Eo "[a-zA-Z0-9.\-]*\->[a-zA-Z0-9.\-]*:")
        avg=( $(echo $line | sed "s|$direction||g" | grep -Eo "[0-9.]*") )
        avg=$(bc -l <<< "scale=4;${avg[1]}/2")
        echo "$direction $avg ms" >> $PING_OUT
    fi
done
