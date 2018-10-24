#!/bin/bash

IP_BASE="192.168.56.10"

# Ping each VM to check if they are up
for i in {2..8}
do
    ping $IP_BASE$i
done