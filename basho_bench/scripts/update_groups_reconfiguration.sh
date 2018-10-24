#!/bin/bash

bucket_file=$1
sudo sed -i -e "s#{saturn_buckets_file.*#{saturn_buckets_file, '$bucket_file'}.#" examples/reconfig_manager.config
