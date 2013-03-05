#!/bin/bash
# https://github.com/spawngrid/kerl
set -e

if [ 'which erl' ]; then
  echo Erlang must be installed before elixir; use install-erlang.sh
  exit 1
fi


git clone https://github.com/elixir-lang/elixir.git /opt/elixir
pushd /opt/elixir
make test
popd

export PATH=$PATH:/opt/elixir/bin

