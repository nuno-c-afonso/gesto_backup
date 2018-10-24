#!/bin/bash

Command0="cd ./saturn_leaf && sudo ./rel/saturn_leaf/bin/saturn_leaf stop"

./scripts/parallel_command_all.sh "$Command0"

Command5="cd ./saturn_leaf && sudo ./rel/saturn_leaf/bin/saturn_leaf start"

./scripts/parallel_command_all.sh "$Command5"
