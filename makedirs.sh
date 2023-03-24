#!/bin/bash

cd examples || exit 1
for d in */; do (cd "$d" && mkdir -p obj && mkdir -p bin); done
cd ..

cd labs/1st-algorithmization-and-programming || exit 1
for d in */; do (cd "$d" && mkdir -p obj && mkdir -p bin); done
cd ../..

cd labs/2nd-algorithms-and-data-structures || exit 1
for d in */; do (cd "$d" && mkdir -p obj && mkdir -p bin); done
cd ../..
