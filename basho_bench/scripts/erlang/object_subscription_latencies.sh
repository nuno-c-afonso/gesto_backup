#!/bin/bash

Leafs=`cat ../leafs`
Ip=`dig +short myip.opendns.com @resolver1.opendns.com`

sudo erl -pa script -name "stat@$Ip" -setcookie saturn_leaf -run stats collect_object_subscription_latencies $Leafs -run init stop
