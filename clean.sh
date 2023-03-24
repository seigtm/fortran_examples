#!/bin/bash

cd examples/1st-algorithmization-and-programming || exit 1
for d in */; do (cd "$d" && make clean); done
cd ../..

cd examples/2nd-algorithms-and-data-structures || exit 1
for d in */; do (cd "$d" && make clean); done
cd ../..

cd labs/1st-algorithmization-and-programming || exit 1
for d in */; do (cd "$d" && make clean); done
cd ../..

cd labs/2nd-algorithms-and-data-structures || exit 1
for d in */; do (cd "$d" && make clean); done
cd ../..
