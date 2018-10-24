#!/bin/bash

#$1 Leaf to send
#$2 internal to read from
#$3 origin operation node
#$4 tyoe of operation (updates|remotes) 

Leafs=`cat ../leafs`
Ip=`dig +short myip.opendns.com @resolver1.opendns.com`
sudo erl -pa script -name "stat@$Ip" -setcookie saturn_leaf -run stats cdf_internal $1 $2 $3 $4 $Leafs -run init stop
