#!/bin/bash

###########
## UTILS ##
###########

YELLOW='\033[43;30m'
RESET='\e[0m'

function alert {
  echo -e "${YELLOW}\n"
  echo $1
  echo -e "${RESET}\n"
}

############
## SCRIPT ##
############

for node in `cat scripts/internals scripts/leafs scripts/bench`
do
	alert $node
	cmd="ssh root@$node '$1'"
	eval $cmd
done
