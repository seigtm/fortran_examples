#!/bin/bash

cd examples || exit 1
for d in */; do (cd "$d" && make clean); done
cd ..

cd labs || exit 1
for d in */; do (cd "$d" && make clean); done
cd ..
