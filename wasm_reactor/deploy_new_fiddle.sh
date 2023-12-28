#!/bin/bash
./build-wasm.sh
GITREV=$(git rev-parse --short HEAD)
cd ../../indigo-fiddle || exit
git commit -m "Update fiddle to indigo@$GITREV" indigo-init.wasm indigo-lib
git push
