#!/bin/bash

sudo rm -rf /home/nca/Desktop/gesto-sandbox/saturn_benchmark/saturn_leaf/deps
sudo rm -rf /home/nca/Desktop/gesto-sandbox/saturn_benchmark/saturn_leaf/ebin
for i in {2..8}
do
	node=192.168.56.10$i
	scp -rp /home/nca/Desktop/gesto-sandbox/saturn_benchmark/saturn_leaf root@$node:.
done
