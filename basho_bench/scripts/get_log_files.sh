#!/bin/bash

cd saturn_leaf/rel/saturn_leaf/log &&
for i in $(ls);
do
  echo -e "\n\n=========================== $i\n\n";
  cat $i;
done
