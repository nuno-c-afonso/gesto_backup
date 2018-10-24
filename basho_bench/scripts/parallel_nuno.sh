#!/bin/bash

for i in {2..8}
do
	node=192.168.56.10$i
	cmd="ssh root@$node '$1'"
	eval $cmd &
done
