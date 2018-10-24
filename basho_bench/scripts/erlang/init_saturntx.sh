#!/bin/bash

Leafs=`cat ../leafs`
Ip=`dig +short myip.opendns.com @resolver1.opendns.com`
#Ip="192.168.56.1"
Internals=`cat ../internals`
LeafsN=0
for node in $Leafs
do
    let LeafsN=LeafsN+1
done
sudo erl -pa script -name "stat@$Ip" -setcookie saturn_leaf -run init_saturn_node init_all_txs $LeafsN $Leafs $Internals -run init stop
