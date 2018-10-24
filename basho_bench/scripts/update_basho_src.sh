#!/bin/bash

update_remote() {
    machine=$1
    ZIP_NAME=$2
    DIR=$3
    users=$4

    # Copy to the other local users
    cmd=""
    for u in $users
    do
        cmd="$cmd cp -r $DIR/* /home/$u/$DIR;"
    done

    # Copy to the machine (as root)
    scp -o StrictHostKeyChecking=no -p $ZIP_NAME.zip $machine:$DIR
    ssh $machine "unzip -o $DIR/$ZIP_NAME.zip -d $DIR; rm $DIR/$ZIP_NAME.zip; $cmd"
}

if [ $# -gt 2 ]
then
    LOCAL_DIR=$1
    DIR=$2
    bench=$3
else
    LOCAL_DIR="basho_bench/src"
    DIR="basho_bench/src"
    bench="172.16.48.16 172.16.130.34 172.16.17.26"
fi

ZIP_NAME=src_basho
users="local eventual causal"
current_dir=$(pwd)

# Create a zip for faster transfer
cd ../$LOCAL_DIR
find -type f -exec touch {} +
zip -r $ZIP_NAME .

for n in $bench
do
    machine=root@$n.g5k
    echo $machine

    # Makes the transfer in parallel
    # sleep 0.2
    sleep 0.5
    update_remote $machine $ZIP_NAME $DIR "$users" &
done

# Wait until there are no more jobs running
running=$(jobs -r | wc -l)
while [ $running -ne 0 ]
do
    running=$(jobs -r | wc -l)
done

# Clean the temporary files
rm $ZIP_NAME.zip
cd $current_dir