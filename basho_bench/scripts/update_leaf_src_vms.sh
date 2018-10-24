#!/bin/bash

DIR="src"
leaves_internal="192.168.56.102 192.168.56.103 192.168.56.104 192.168.56.105"

if [ $# -gt 0 ]
then
    LOCAL_DIR=$1
else
    LOCAL_DIR="gesto_partial"
fi

ZIP_NAME=src_leaf
current_dir=$(pwd)

# Create a zip for faster transfer
cd ../$LOCAL_DIR/$DIR
find -type f -exec touch {} +
cd ..
zip -r $ZIP_NAME $DIR

# Copy the changes to each server
for n in $leaves_internal
do
    machine=root@$n
    echo $machine

    # Copy to the machine (as root)
    scp -p $ZIP_NAME.zip $machine:.
    ssh $machine "unzip -o $ZIP_NAME.zip; cp -r $DIR saturn_leaf; rm -rf $DIR; rm $ZIP_NAME.zip" &
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