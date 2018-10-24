#!/bin/bash

Leafs=`cat ./scripts/leafs`
Internals=`cat ./scripts/internals`

erl -pa script -setcookie saturn_leaf -name setup@localhost -run init_saturn_node test1 $Leafs $Internals -run init stop
