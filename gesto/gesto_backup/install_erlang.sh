#!/bin/bash

# Store the original directory
current_dir=$(pwd)
cd ~/Downloads

sudo apt-get update

# Install dependencies
sudo apt-get install -y build-essential autoconf libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev git
sudo apt-get install -y libwxbase2.8 libwxgtk2.8-dev libqt4-opengl-dev

# Install Erlang
wget http://s3.amazonaws.com/downloads.basho.com/erlang/otp_src_R16B02-basho10.tar.gz
tar zxvf otp_src_R16B02-basho10.tar.gz
cd OTP_R16B02_basho10
./otp_build autoconf
./configure && make && sudo make install

# Clean temporary files
cd ~/Downloads
rm otp_src_R16B02-basho10.tar.gz
rm -rf OTP_R16B02_basho10


# Return to the original directory
cd $current_dir
