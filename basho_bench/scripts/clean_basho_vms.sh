#!/bin/bash

IP_BASE="192.168.56.10"
USERS="root causal eventual local"

# Kills all the Erlang processes
for i in {6..8}
do

    for u in $USERS
    do
        machine="$u@$IP_BASE$i"
        ssh $machine "cd basho_bench; sudo make clean; sudo rm -r deps" &
    done
done

# Wait until there are no more jobs running
running=$(jobs -r | wc -l)
while [ $running -ne 0 ]
do
    running=$(jobs -r | wc -l)
done