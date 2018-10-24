#!/bin/bash

# This script must be stored in Lille!

if [ $# -lt 1 ]
then    
    echo "It must receive the image name as an argument!"
    exit
fi

image_name=$1

# Get locations
locations="grenoble luxembourg lyon nancy nantes rennes sophia"

for location in $locations
do
    rm $location/$image_name* &
done