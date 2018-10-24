#!/bin/bash

for i in {2..8}
do
	node=192.168.56.10$i
	cmd="scp -rp $1 root@$node:."
	eval $cmd &
done
