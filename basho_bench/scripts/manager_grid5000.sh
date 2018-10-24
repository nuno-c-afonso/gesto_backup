#!/bin/bash

echo "I'M THE MANAGER!"

###############################
# SAME AS prepare_grid5000.sh #
###############################

USER="nuafonso"
my_location=$1
walltime=$2
machine=$3
addr=$USER@$my_location

# Get JobId
jobid=$(ssh $addr "oarsub -t deploy -p \"cluster='$machine'\" -l nodes=1,walltime=$walltime \"sleep 5d\" | grep OAR_JOB_ID | grep -Eo \"[0-9]*\"")
echo "JOBID: $jobid"

# Get hostname
hostname=''
while [[ -z "${hostname// }" ]]
do
    sleep 1
    hostname=($(ssh $addr "oarstat -j $jobid -p | grep -Eo \"host = '.+'\" | grep -oP \"(?<=').*?(?=')\""))
    hostname=${hostname[0]}
done
echo "HOSTNAME: $hostname"

ssh $USER@$my_location "kadeploy3 -m $hostname -a ubuntu1404_ssh-x64-min.env -k"

#################
# MANAGER STUFF #
#################

# Copy ssh private key to the manager machine
machine=root@$hostname.g5k
KEYNAME=grid5000_internal
SCRIPTS=basho_bench/scripts
RESULTS=basho_bench/results

scp ../../ssh_grid5000/$KEYNAME $machine:.ssh
cmd="eval \"\$(ssh-agent -s)\"; ssh-add .ssh/$KEYNAME"
cmd="$cmd; nano $SCRIPTS/internals; nano $SCRIPTS/leafs; nano $SCRIPTS/bench"
cmd="$cmd; mkdir $RESULTS"
cmd="$cmd; bash -c 'cd basho_bench; scripts/ping_grid5000.sh; rm results/*; scripts/ping_grid5000.sh; bash'"
ssh -t $machine $cmd
ssh -t $machine "cd $RESULTS; zip -r results ."
scp $machine:$RESULTS/results.zip ../grid5000_res
scripts/stop_grid5000.sh
unzip -o ../grid5000_res/results.zip -d ../grid5000_res
rm ../grid5000_res/results.zip
