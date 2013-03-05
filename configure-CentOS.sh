#!/bin/bash
# install requirements for CentOS
# by Patrick Wyatt
set -e

sudo yum install -y python python-devel
sudo yum install -y zeromq zeromq-devel
sudo yum install -y redis redis-devel
sudo yum install -y gnuplot

# Install pip
mkdir -p install-pip
cd install-pip
curl -O https://raw.github.com/pypa/virtualenv/master/virtualenv.py
python virtualenv.py /opt/py_virtual
cd ..
rm -rf install-pip

source /opt/py_virtual/bin/activate
pip install pyzmq
pip install redis
pip install hiredis
pip install argparse

# Install go
rm -r /usr/local/go
wget http://go.googlecode.com/files/go1.0.3.linux-amd64.tar.gz
tar -C /usr/local -xzf go1.0.3.linux-amd64.tar.gz
rm go1.0.3.linux-amd64.tar.gz

go get github.com/garyburd/redigo/redis
go get github.com/alecthomas/gozmq

export PATH=$PATH:/usr/local/go/bin

