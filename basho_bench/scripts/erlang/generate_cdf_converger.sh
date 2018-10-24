#!/bin/bash


Leafs=`cat ../leafs`
Ip=`dig +short myip.opendns.com @resolver1.opendns.com`
sudo erl -pa script -name "stat@$Ip" -setcookie saturn_leaf -run stats cdf_converger $1 $2 $3 $Leafs -run init stop
