#!/bin/bash

set -e

nix-build --arg doCheck false   --arg disableOptimization false   --arg enableDeadCodeElimination true   --arg doBenchmark true   --arg doStrip false   --arg enableLibraryProfiling true   --arg enableExecutableProfiling true   --arg doTracing true   --arg enableDWARFDebugging true
result/bin/hnix --check data/nix/tests/lang/eval-okay-let.nix
