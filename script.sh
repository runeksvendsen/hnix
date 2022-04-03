#!/bin/bash

commit=$1

function no_ctrlc()
{
  exit 1
}
trap no_ctrlc SIGINT


git checkout $commit
echo -n "$commit " >> buildlog.log
nix-shell --arg doCheck false --arg doStrip false --arg disableOptimization true --run 'cabal v2-run hnix -- --check ./data/nix/tests/lang/eval-okay-let.nix|grep "<<loop>>" >> buildlog.log'

