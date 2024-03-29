#!/bin/bash
# TODO: Make sure this script is always run from wasm_reactor directory
wasm32-wasi-cabal build exe:indigo-wasm-reactor --allow-newer='*' -f noffi
INDIGO_WASM=$(realpath ./dist-newstyle/build/wasm32-wasi/ghc-*/indigo-wasm-reactor-*/x/indigo-wasm-reactor/build/indigo-wasm-reactor/indigo-wasm-reactor.wasm)
wizer --allow-wasi --wasm-bulk-memory true "$INDIGO_WASM" -o indigo-init.wasm
cp -f indigo-init.wasm ../../indigo-fiddle
cp -rf ../share ../../indigo-fiddle/indigo-lib
