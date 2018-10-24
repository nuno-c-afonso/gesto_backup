#!/bin/bash

Leafs=`cat ../leafs`
LeafsN=0
Ip=`dig +short myip.opendns.com @resolver1.opendns.com`
for node in $Leafs
do
    let LeafsN=LeafsN+1
done
sudo erl -pa script -name "stat@$Ip" -setcookie saturn_leaf -run init_saturn_node init_all_eventual $LeafsN $Leafs -run init stop
#sudo erl -pa script -name "stat@66.249.78.57" -setcookie saturn_leaf -run init_saturn_node init_all_eventual $LeafsN $Leafs -run init stop
