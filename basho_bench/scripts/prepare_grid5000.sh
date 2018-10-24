#!/bin/bash

USER="nuafonso"
my_location=$1
walltime=$2
machine=$3
addr=$USER@$my_location

# Get JobId
# jobid=$(ssh $addr "oarsub -t deploy -p \"cluster='${clusters[$my_location]}'\" -l nodes=1,walltime=$walltime \"sleep 5d\" | grep OAR_JOB_ID | grep -Eo \"[0-9]*\"")
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

sleep 5
ssh $addr "kadeploy3 -m $hostname -a ubuntu1404_ssh-x64-min.env -k"

# Show IP address for updating the benchmark files
machine=root@$hostname.g5k

# Four different users (root is the default one)
ssh $machine "echo '$(cat ../../ssh_grid5000/grid5000_internal.pub)' >> ~/.ssh/authorized_keys"

ssh $machine "mkdir /home/local/.ssh; mkdir /home/causal/.ssh; mkdir /home/eventual/.ssh"
ssh $machine "echo '$(cat ../../ssh_grid5000/grid5000_internal.pub)' > /home/local/.ssh/authorized_keys"
ssh $machine "echo '$(cat ../../ssh_grid5000/grid5000_internal.pub)' > /home/causal/.ssh/authorized_keys"
ssh $machine "echo '$(cat ../../ssh_grid5000/grid5000_internal.pub)' > /home/eventual/.ssh/authorized_keys"

ssh $machine "ifconfig; sleep 300"
ssh -t $machine "sudo apt install htop && htop"
ssh $machine
