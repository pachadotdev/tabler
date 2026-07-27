#!/bin/bash
set -e

# Setup Emscripten SDK
cd ~/Documents
sudo pacman -S sdl2 sdl2_mixer sdl2_net
if [ ! -d "emsdk" ]; then
    git clone --depth 1 git@github.com:emscripten-core/emsdk.git
fi
cd emsdk
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh

# Build Doom WASM with legacy C standard flags
cd ~/Documents
if [ ! -d "doom-wasm" ]; then
    git clone --depth 1 git@github.com:cloudflare/doom-wasm.git
fi
cd doom-wasm

./scripts/clean.sh
./scripts/build.sh

wget https://github.com/nneonneo/universal-doom/raw/refs/heads/main/DOOM1.WAD -O src/doom1.wad

mkdir doom-compiled
mv src/index.html doom-compiled/index.html
mv src/doom1.wad doom-compiled/doom1.wad
mv src/default.cfg doom-compiled/default.cfg
mv src/websockets-doom.js doom-compiled/websockets-doom.js
mv src/websockets-doom.html doom-compiled/websockets-doom.html
mv src/websockets-doom.wasm doom-compiled/websockets-doom.wasm
mv src/websockets-doom.wasm.map doom-compiled/websockets-doom.wasm.map
