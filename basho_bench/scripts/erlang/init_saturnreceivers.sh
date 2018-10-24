#!/bin/bash

Leafs=`cat ../leafs`
Ip=`dig +short myip.opendns.com @resolver1.opendns.com`
Internals=`cat ../internals`
Receivers=`cat ../receivers`
LeafsN=0
for node in $Leafs
do
    let LeafsN=LeafsN+1
done
sudo erl -pa script -name "stat@$Ip" -setcookie saturn_leaf -run init_saturn_node init_all_receivers $LeafsN $Leafs $Receivers $Internals -run init stop
