#!/bin/bash

LOCAL_DIR="basho_bench"
DIR="src"
bench="192.168.56.106 192.168.56.107 192.168.56.108"

ZIP_NAME=src_basho
users="local eventual causal"
current_dir=$(pwd)

# Create a zip for faster transfer
cd ../$LOCAL_DIR/$DIR
find -type f -exec touch {} +
cd ..
zip -r $ZIP_NAME $DIR

# FIXME: There is some bug that consumes a lot of disk space. VMs got full to the max.

for n in $bench
do
    machine=root@$n
    echo $machine
        
    # Copy to the machine (as root)
    scp -p $ZIP_NAME.zip $machine:.
    ssh $machine "unzip -o $ZIP_NAME.zip; cp -r $DIR basho_bench; rm -rf $DIR; rm $ZIP_NAME.zip"

    # Copy to the other local users
    for u in $users
    do
        ssh $machine "cp -r basho_bench /home/$u" &
    done
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
