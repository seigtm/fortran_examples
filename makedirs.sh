#!/bin/bash

cd examples || exit 1
for d in */; do (cd "$d" && mkdir -p obj && mkdir -p bin); done
cd ..

cd labs || exit 1
for d in */; do (cd "$d" && mkdir -p obj && mkdir -p bin); done
cd ..
