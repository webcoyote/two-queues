#!/bin/bash
# https://github.com/spawngrid/kerl
set -e

curl -O https://raw.github.com/spawngrid/kerl/master/kerl
chmod +x kerl
mv kerl /opt/

kerl build R16A_RELEASE_CANDIDATE r16rc
kerl install r16rc /opt/erlang-r16rc
. /opt/erlang-r16rc/activate

