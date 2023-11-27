#!/bin/bash
./build-wasm.sh
cd ../../indigo-fiddle || exit
git commit -m "Deploying new fiddle" indigo-init.wasm indigo-lib
git push
