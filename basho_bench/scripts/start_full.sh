#!/bin/bash

Command0="cd ./saturn_leaf && sudo ./rel/saturn_leaf/bin/saturn_leaf stop"

./scripts/parallel_command_all.sh "$Command0"

if [ $# -eq 1 ]
then
    branch=$1
    Command1="cd ./saturn_leaf/ && git reset --hard && git fetch && git checkout $branch && git pull"
else
    Command1="cd ./saturn_leaf && git pull origin master"
fi

./scripts/parallel_command_all.sh "$Command1"

./scripts/change_nodename.sh leafs

./scripts/change_nodename.sh internals

./scripts/change_nodename.sh receivers 

Command3="cd ./saturn_leaf && sudo make relclean"

./scripts/parallel_command_all.sh "$Command3"

Command4="cd ./saturn_leaf && make rel"

./scripts/parallel_command_all.sh "$Command4"

Command5="cd ./saturn_leaf && sudo ./rel/saturn_leaf/bin/saturn_leaf start"

./scripts/parallel_command_all.sh "$Command5"
