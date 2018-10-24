#!/bin/bash

Command0="cd ./saturn_leaf && sudo ./rel/saturn_leaf/bin/saturn_leaf stop"

./scripts/parallel_command_all.sh "$Command0"

Command3="cd ./saturn_leaf && sudo make relclean"

./scripts/parallel_command_all.sh "$Command3"

Command4="cd ./saturn_leaf && make rel"

./scripts/parallel_command_all.sh "$Command4"

Command5="cd ./saturn_leaf && sudo ./rel/saturn_leaf/bin/saturn_leaf start"

./scripts/parallel_command_all.sh "$Command5"
