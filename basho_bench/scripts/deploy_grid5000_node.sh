#!/bin/bash

cmd='oarsub -I -l nodes=1 -t deploy; kadeploy3 -f $OAR_NODE_FILE -e ubuntu1404-x64-min -k'
echo $cmd
#ssh -t nuafonso@lille $cmd
