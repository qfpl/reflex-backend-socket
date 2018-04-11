#! /usr/bin/env bash

nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
nix-prefetch-git https://github.com/dalaing/reflex-basic-host > reflex-basic-host.json
nix-prefetch-git https://github.com/dalaing/reflex-binary > reflex-binary.json
