#!/bin/bash

IP_BASE="192.168.56.10"

# Kills all the Erlang processes
for i in {2..8}
do
    machine="root@$IP_BASE$i"
    ssh $machine "pkill -f erl" &
done

# Wait until there are no more jobs running
running=$(jobs -r | wc -l)
while [ $running -ne 0 ]
do
    running=$(jobs -r | wc -l)
done