#!/bin/bash

Leafs=`cat ../leafs`
Internals=`cat ../internals`
LeafsN=0
for node in $Leafs
do
    let LeafsN=LeafsN+1
done
Counter=0
for node in $Leafs
do
    let Counter=$Counter+1
    NodeName="leafs$Counter@$node"
    let Id=$Counter-1
    #echo "$NodeName"
    sudo erl -pa script -name "stat@130.104.228.60" -setcookie saturn_leaf -run init_saturn_node init $NodeName leafs $Id $LeafsN $Leafs $Internals -run init stop
done
let Counter=0
for node in $Internals
do
    let Counter=$Counter+1
    let Id=$Id+1
    NodeName="internals$Counter@$node"
    #echo "$NodeName"
    sudo erl -pa script -name "stat@130.104.228.60" -setcookie saturn_leaf -run init_saturn_node init $NodeName internals $Id $LeafsN $Leafs $Internals -run init stop
done
